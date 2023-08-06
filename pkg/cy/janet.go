package cy

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/cy/api"
	"github.com/cfoust/cy/pkg/fuzzy"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/util"

	"github.com/rs/zerolog/log"
)

import _ "embed"

//go:embed cy-boot.janet
var CY_BOOT_FILE []byte

type CmdParams struct {
	Command string
	Args    []string
}

func (c *Cy) initJanet(ctx context.Context, configFile string) (*janet.VM, error) {
	vm, err := janet.New(ctx)
	if err != nil {
		return nil, err
	}

	modules := map[string]interface{}{
		"cmd": &api.Cmd{
			Lifetime: util.NewLifetime(c.Ctx()),
			Tree:     c.tree,
		},
		"group": &api.GroupModule{Tree: c.tree},
		"pane":  &api.PaneModule{Tree: c.tree},
		"path":  &api.PathModule{},
		"tree":  &api.TreeModule{Tree: c.tree},
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
		"cy/copy-mode": func(user interface{}) {
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

			term, ok := pane.Screen().(*screen.Terminal)
			if !ok {
				return
			}

			copyMode := screen.NewCopyMode(
				ctx,
				client.info,
				term.History(),
				geom.DEFAULT_SIZE,
			)

			client.innerLayers.NewLayer(
				copyMode.Ctx(),
				copyMode,
				true,
				true,
			)
		},
		"log": func(text string) {
			c.log.Info().Msgf(text)
		},
		"fzf/find": func(
			ctx context.Context,
			user interface{},
			choices *janet.Value,
		) (interface{}, error) {
			client, ok := user.(*Client)
			if !ok {
				return nil, fmt.Errorf("missing client context")
			}

			options, err := fuzzy.UnmarshalOptions(choices)
			if err != nil {
				return nil, err
			}

			cursor := client.outerLayers.State().Cursor
			fuzzy := fuzzy.NewFuzzy(
				ctx,
				client.info,
				options,
				geom.Vec2{
					R: cursor.Y,
					C: cursor.X,
				},
			)

			client.outerLayers.NewLayer(
				fuzzy.Ctx(),
				anim.NewAnimator(
					fuzzy.Ctx(),
					&anim.Cyform{},
					client.outerLayers.State().Image,
					23,
				),
				false,
				true,
			)

			client.outerLayers.NewLayer(
				fuzzy.Ctx(),
				fuzzy,
				true,
				false,
			)

			select {
			case result := <-fuzzy.Result():
				return result, nil
			case <-ctx.Done():
				return nil, ctx.Err()
			}
		},
		"key/bind": func(sequence []string, doc string, callback *janet.Function) error {
			c.tree.Root().Binds().Set(
				sequence,
				tree.Binding{
					Description: doc,
					Callback:    callback,
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
			log.Info().Msgf("%+v", size)
			client.margins.SetSize(size)
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

	if len(configFile) != 0 {
		err := vm.ExecuteFile(ctx, configFile)
		if err != nil {
			return nil, err
		}
	}

	return vm, nil
}
