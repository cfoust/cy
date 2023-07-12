package cy

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/wm"

	"github.com/rs/zerolog/log"
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
			log.Info().Msgf(text)
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
		"group/root": func() wm.NodeID {
			return c.tree.Root().Id()
		},
		"group/new": func(parentId wm.NodeID) (wm.NodeID, error) {
			node, ok := c.tree.NodeById(parentId)
			if !ok {
				return 0, fmt.Errorf("node not found: %d", parentId)
			}

			group, ok := node.(*wm.Group)
			if !ok {
				return 0, fmt.Errorf("node was not group")
			}

			return group.NewGroup().Id(), nil
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
		"pane/path": func(id wm.NodeID) (*string, error) {
			node, ok := c.tree.NodeById(id)
			if !ok {
				return nil, fmt.Errorf("node not found: %d", id)
			}

			pane, ok := node.(*wm.Pane)
			if !ok {
				return nil, fmt.Errorf("node was not pane")
			}

			path, err := pane.Path()
			if err != nil {
				return nil, err
			}

			return &path, nil
		},
		"pane/new": func(groupId wm.NodeID, path string) (wm.NodeID, error) {
			node, ok := c.tree.NodeById(groupId)
			if !ok {
				return 0, fmt.Errorf("node not found: %d", groupId)
			}

			group, ok := node.(*wm.Group)
			if !ok {
				return 0, fmt.Errorf("node was not group")
			}

			return group.NewPane(
				c.Ctx(),
				wm.PaneOptions{
					Command:   "/bin/bash",
					Directory: path,
				},
				wm.DEFAULT_SIZE,
			).Id(), nil
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
