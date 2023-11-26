package cy

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/cy/api"
	"github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/janet"
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

func (c *Cy) initJanet(ctx context.Context, dataDir string) (*janet.VM, error) {
	vm, err := janet.New(ctx)
	if err != nil {
		return nil, err
	}

	modules := map[string]interface{}{
		"cmd": &api.Cmd{
			Lifetime:    util.NewLifetime(c.Ctx()),
			Tree:        c.tree,
			ReplayBinds: c.replayBinds,
		},
		"key": &api.Key{
			Tree:        c.tree,
			ReplayBinds: c.replayBinds,
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
		"input": &api.InputModule{
			Tree:   c.tree,
			Server: c.muxServer,
		},
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
		"cy/get": func(user interface{}, key *janet.Value) (interface{}, error) {
			defer key.Free()

			var keyword janet.Keyword
			err := key.Unmarshal(&keyword)
			if err != nil {
				return nil, err
			}

			client, ok := user.(*Client)
			if !ok {
				return nil, fmt.Errorf("missing client context")
			}

			// First check the client's parameters
			value, ok := client.params.Get(string(keyword))
			if ok {
				return value, nil
			}

			// Then those found in the tree
			node := client.Node()
			if node == nil {
				return nil, fmt.Errorf("client was not attached")
			}

			value, ok = node.Params().Get(string(keyword))
			return value, nil
		},
		"cy/set": func(user interface{}, key *janet.Value, value *janet.Value) error {
			defer key.Free()

			client, ok := user.(*Client)
			if !ok {
				return fmt.Errorf("missing client context")
			}

			node := client.Node()
			if node == nil {
				return fmt.Errorf("client was not attached")
			}

			var keyword janet.Keyword
			err := key.Unmarshal(&keyword)
			if err != nil {
				return err
			}

			var str string
			err = value.Unmarshal(&str)
			if err == nil {
				node.Params().Set(string(keyword), str)
				return nil
			}

			var _int int
			err = value.Unmarshal(&_int)
			if err == nil {
				node.Params().Set(string(keyword), _int)
				return nil
			}

			var _bool int
			err = value.Unmarshal(&_bool)
			if err == nil {
				node.Params().Set(string(keyword), _bool)
				return nil
			}

			return fmt.Errorf("parameter type not supported")
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
		"frame/set": func(context interface{}, name string) {
			client, ok := context.(*Client)
			if !ok {
				return
			}

			frame, ok := frames.Frames[name]
			if !ok {
				return
			}
			client.frame.Set(frame)
		},
		"frame/get-all": func(context interface{}) []string {
			var names []string
			for name := range frames.Frames {
				names = append(names, name)
			}
			return names
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
