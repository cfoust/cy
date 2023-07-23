package cy

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/fuzzy"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/mux/stream"
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

	callbacks := map[string]interface{}{
		"log": func(text string) {
			c.log.Info().Msgf(text)
		},
		"cmd/new": func(
			groupId tree.NodeID,
			path string,
			params *janet.Named[CmdParams],
		) (tree.NodeID, error) {
			values := params.WithDefault(CmdParams{
				Command: "/bin/bash",
			})

			group, ok := c.tree.GroupById(groupId)
			if !ok {
				return 0, fmt.Errorf("node not found: %d", groupId)
			}

			cmd, err := group.NewCmd(
				c.Ctx(),
				stream.CmdOptions{
					Command:   values.Command,
					Args:      values.Args,
					Directory: path,
				},
				geom.DEFAULT_SIZE,
			)
			if err != nil {
				return 0, err
			}

			return cmd.Id(), nil
		},
		"cmd/path": func(id tree.NodeID) (*string, error) {
			pane, ok := c.tree.PaneById(id)
			if !ok {
				return nil, fmt.Errorf("pane not found: %d", id)
			}

			cmd, ok := pane.App().(*stream.Cmd)
			if !ok {
				return nil, fmt.Errorf("pane was not a cmd")
			}

			path, err := cmd.Path()
			if err != nil {
				return nil, err
			}

			return &path, nil
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

			fuzzy := fuzzy.NewFuzzy(
				ctx,
				client.colorProfile,
				client.info,
				options,
			)

			client.layers.NewLayer(
				fuzzy.Ctx(),
				fuzzy,
				true,
			)

			select {
			case result := <-fuzzy.Result():
				return result, nil
			case <-ctx.Done():
				return nil, ctx.Err()
			}
		},
		"group/new": func(parentId tree.NodeID) (tree.NodeID, error) {
			group, ok := c.tree.GroupById(parentId)
			if !ok {
				return 0, fmt.Errorf("node not found: %d", parentId)
			}

			return group.NewGroup().Id(), nil
		},
		"group/children": func(parentId tree.NodeID) ([]tree.NodeID, error) {
			group, ok := c.tree.GroupById(parentId)
			if !ok {
				return nil, fmt.Errorf("node not found: %d", parentId)
			}

			nodes := make([]tree.NodeID, 0)
			for _, child := range group.Children() {
				nodes = append(nodes, child.Id())
			}
			return nodes, nil
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
		"pane/attach": func(context interface{}, id tree.NodeID) error {
			client, ok := context.(*Client)
			if !ok {
				return fmt.Errorf("missing client context")
			}

			node, ok := c.tree.NodeById(id)
			if !ok {
				return fmt.Errorf("node not found: %d", id)
			}

			return client.Attach(node)
		},
		"pane/current": func(context interface{}) *tree.NodeID {
			client, ok := context.(*Client)
			if !ok {
				return nil
			}

			node := client.node
			if node == nil {
				return nil
			}

			id := node.Id()
			return &id
		},
		"tree/group?": func(id tree.NodeID) bool {
			_, ok := c.tree.GroupById(id)
			return ok
		},
		"tree/pane?": func(id tree.NodeID) bool {
			_, ok := c.tree.PaneById(id)
			return ok
		},
		"tree/set-name": func(id tree.NodeID, name string) {
			node, ok := c.tree.NodeById(id)
			if !ok {
				return
			}

			node.SetName(name)
		},
		"tree/name": func(id tree.NodeID) *string {
			node, ok := c.tree.NodeById(id)
			if !ok {
				return nil
			}

			name := node.Name()
			return &name
		},
		"tree/parent": func(id tree.NodeID) *tree.NodeID {
			node, ok := c.tree.NodeById(id)
			if !ok {
				return nil
			}

			path := c.tree.PathTo(node)
			if len(path) <= 1 {
				return nil
			}

			parentId := path[len(path)-2].Id()
			return &parentId
		},
		"tree/root": func() tree.NodeID {
			return c.tree.Root().Id()
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
