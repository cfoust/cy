package cy

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/cy/api"
	"github.com/cfoust/cy/pkg/fuzzy"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/replayable"
	"github.com/cfoust/cy/pkg/mux/screen/toasts"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/util"
)

import _ "embed"

//go:embed cy-boot.janet
var CY_BOOT_FILE []byte

// execute runs some Janet code on behalf of the Client. This is only used in testing.
func (c *Client) execute(code string) error {
	return c.cy.janet.ExecuteCall(c.Ctx(), c, janet.Call{
		Code:    []byte(code),
		Options: janet.DEFAULT_CALL_OPTIONS,
	})
}

var (
	KEYWORD_ROOT   = janet.Keyword("root")
	KEYWORD_REPLAY = janet.Keyword("replay")

	KEYWORD_INFO  = janet.Keyword("info")
	KEYWORD_WARN  = janet.Keyword("warn")
	KEYWORD_ERROR = janet.Keyword("error")
)

func resolveLevel(level *janet.Value) (toasts.ToastLevel, error) {
	err := level.Unmarshal(&KEYWORD_INFO)
	if err == nil {
		return toasts.ToastLevelInfo, nil
	}

	err = level.Unmarshal(&KEYWORD_WARN)
	if err == nil {
		return toasts.ToastLevelWarn, nil
	}

	err = level.Unmarshal(&KEYWORD_ERROR)
	if err == nil {
		return toasts.ToastLevelError, nil
	}

	return toasts.ToastLevelError, fmt.Errorf("you must provide one of :info, :warn, or :error")
}

func (c *Cy) resolveGroup(target *janet.Value) (*tree.Group, error) {
	// first try keyword
	err := target.Unmarshal(&KEYWORD_ROOT)
	if err == nil {
		return c.tree.Root(), nil
	}

	// otherwise, node ID
	var id tree.NodeID
	if err := target.Unmarshal(&id); err != nil {
		return nil, err
	}

	group, ok := c.tree.GroupById(id)
	if !ok {
		return nil, fmt.Errorf("group not found: %d", id)
	}

	return group, nil
}

func (c *Cy) initJanet(ctx context.Context, dataDir string) (*janet.VM, error) {
	vm, err := janet.New(ctx)
	if err != nil {
		return nil, err
	}

	modules := map[string]interface{}{
		"cmd": &api.Cmd{
			Lifetime: util.NewLifetime(c.Ctx()),
			DataDir:  dataDir,
			Tree:     c.tree,
		},
		"group": &api.GroupModule{Tree: c.tree},
		"pane":  &api.PaneModule{Tree: c.tree},
		"path":  &api.PathModule{},
		"replay": &api.ReplayModule{
			Lifetime: util.NewLifetime(c.Ctx()),
			Tree:     c.tree,
			Binds:    c.replayBinds,
		},
		"tree": &api.TreeModule{Tree: c.tree},
	}

	for name, module := range modules {
		err := vm.Module(name, module)
		if err != nil {
			return nil, err
		}
	}

	callbacks := map[string]interface{}{
		"cy/kill-server": func() {
			c.Shutdown()
		},
		"cy/detach": func(user interface{}) {
			client, ok := user.(*Client)
			if !ok {
				return
			}

			client.Detach("detached")
		},
		"cy/replay": func(user interface{}) {
			client, ok := user.(*Client)
			if !ok {
				return
			}

			node := client.Node()
			if node == nil {
				return
			}

			pane, ok := node.(*tree.Pane)
			if !ok {
				return
			}

			r, ok := pane.Screen().(*replayable.Replayable)
			if !ok {
				return
			}

			r.EnterReplay()
			// TODO(cfoust): 10/08/23 reattach all clients
			client.Attach(node)
		},
		"cy/paste": func(user interface{}) {
			client, ok := user.(*Client)
			if !ok {
				return
			}

			buffer := client.buffer
			if len(buffer) == 0 {
				return
			}

			client.binds.Input([]byte(buffer))
		},
		"log": func(text string) {
			c.log.Info().Msgf(text)
		},
		"fzf/find": func(
			ctx context.Context,
			user interface{},
			choices *janet.Value,
		) (interface{}, error) {
			defer choices.Free()

			client, ok := user.(*Client)
			if !ok {
				return nil, fmt.Errorf("missing client context")
			}

			options, err := fuzzy.UnmarshalOptions(choices)
			if err != nil {
				return nil, err
			}

			outerLayers := client.outerLayers
			state := outerLayers.State()
			cursor := state.Cursor
			result := make(chan interface{})
			fuzzy := fuzzy.NewFuzzy(
				ctx,
				state.Image,
				options,
				geom.Vec2{R: cursor.Y, C: cursor.X},
				c.tree,
				c.muxServer.AddClient(ctx, geom.Vec2{}),
				result,
			)

			client.outerLayers.NewLayer(
				fuzzy.Ctx(),
				fuzzy,
				screen.PositionTop,
				screen.WithInteractive,
				screen.WithOpaque,
			)

			select {
			case match := <-result:
				return match, nil
			case <-ctx.Done():
				return nil, ctx.Err()
			}
		},
		"key/bind": func(target *janet.Value, sequence []string, callback *janet.Function) error {
			defer target.Free()

			var scope *bind.BindScope
			group, err := c.resolveGroup(target)
			if err == nil {
				scope = group.Binds()
			} else {
				replayErr := target.Unmarshal(&KEYWORD_REPLAY)
				if replayErr != nil {
					return fmt.Errorf("target must be one of :root, :replay, or node ID")
				}

				scope = c.replayBinds
			}

			scope.Set(
				sequence,
				bind.Action{
					Callback: callback,
				},
			)

			return nil
		},
		"frame/size": func(context interface{}) *geom.Vec2 {
			client, ok := context.(*Client)
			if !ok {
				return nil
			}
			size := client.margins.Size()
			return &size
		},
		"frame/set-size": func(context interface{}, size geom.Size) {
			client, ok := context.(*Client)
			if !ok {
				return
			}
			client.margins.SetSize(size)
		},
		"cy/toast": func(context interface{}, level *janet.Value, message string) error {
			defer level.Free()

			toastLevel, err := resolveLevel(level)
			if err != nil {
				return err
			}

			client, ok := context.(*Client)
			if !ok {
				return fmt.Errorf("unable to detect client")
			}

			client.toaster.Send(toasts.Toast{
				Level:   toastLevel,
				Message: message,
			})
			return nil
		},
	}

	for name, callback := range callbacks {
		err := vm.Callback(name, callback)
		if err != nil {
			return nil, err
		}
	}

	err = vm.ExecuteCall(ctx, nil, janet.Call{
		Code:       CY_BOOT_FILE,
		SourcePath: "cy-boot.janet",
		Options:    janet.DEFAULT_CALL_OPTIONS,
	})
	if err != nil {
		return nil, err
	}

	return vm, nil
}
