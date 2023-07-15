package cy

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/ui/io"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/wm"
)

import _ "embed"

//go:embed cy-boot.janet
var CY_BOOT_FILE []byte

func (c *Cy) initJanet(ctx context.Context, configFile string) (*janet.VM, error) {
	vm, err := janet.New(ctx)
	if err != nil {
		return nil, err
	}

	callbacks := map[string]interface{}{
		"log": func(text string) {
			c.log.Info().Msgf(text)
		},
		"key/bind": func(sequence []string, doc string, callback *janet.Function) error {
			c.tree.Root().Binds().Set(
				sequence,
				wm.Binding{
					Description: doc,
					Callback:    callback,
				},
			)

			return nil
		},
		"tree/root": func() wm.NodeID {
			return c.tree.Root().Id()
		},
		"tree/parent": func(id wm.NodeID) *wm.NodeID {
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
		"tree/group?": func(id wm.NodeID) bool {
			_, ok := c.tree.GroupById(id)
			return ok
		},
		"tree/pane?": func(id wm.NodeID) bool {
			_, ok := c.tree.PaneById(id)
			return ok
		},
		"group/new": func(parentId wm.NodeID) (wm.NodeID, error) {
			group, ok := c.tree.GroupById(parentId)
			if !ok {
				return 0, fmt.Errorf("node not found: %d", parentId)
			}

			return group.NewGroup().Id(), nil
		},
		"group/children": func(parentId wm.NodeID) ([]wm.NodeID, error) {
			group, ok := c.tree.GroupById(parentId)
			if !ok {
				return nil, fmt.Errorf("node not found: %d", parentId)
			}

			nodes := make([]wm.NodeID, 0)
			for _, child := range group.Children() {
				nodes = append(nodes, child.Id())
			}
			return nodes, nil
		},
		"pane/current": func(context interface{}) *wm.NodeID {
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
		"cmd/path": func(id wm.NodeID) (*string, error) {
			pane, ok := c.tree.PaneById(id)
			if !ok {
				return nil, fmt.Errorf("pane not found: %d", id)
			}

			cmd, ok := pane.App().(*io.Cmd)
			if !ok {
				return nil, fmt.Errorf("pane was not a cmd")
			}

			path, err := cmd.Path()
			if err != nil {
				return nil, err
			}

			return &path, nil
		},
		"cmd/new": func(groupId wm.NodeID, path string) (wm.NodeID, error) {
			group, ok := c.tree.GroupById(groupId)
			if !ok {
				return 0, fmt.Errorf("node not found: %d", groupId)
			}

			cmd, err := group.NewCmd(
				c.Ctx(),
				io.CmdOptions{
					Command:   "/bin/bash",
					Directory: path,
				},
				geom.DEFAULT_SIZE,
			)
			if err != nil {
				return 0, err
			}

			return cmd.Id(), nil
		},
		"pane/attach": func(context interface{}, id wm.NodeID) error {
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
	}

	for name, callback := range callbacks {
		err := vm.Callback(name, callback)
		if err != nil {
			return nil, err
		}
	}

	err = vm.ExecuteCall(janet.Call{
		Code:       CY_BOOT_FILE,
		SourcePath: "cy-boot.janet",
		Options:    janet.DEFAULT_CALL_OPTIONS,
	})
	if err != nil {
		return nil, err
	}

	if len(configFile) != 0 {
		err := vm.ExecuteFile(configFile)
		if err != nil {
			return nil, err
		}
	}

	return vm, nil
}
