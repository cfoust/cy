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

	err = vm.Callback(
		"log",
		func(text string) {
			log.Info().Msgf(text)
		},
	)

	err = vm.Callback(
		"key/bind",
		func(sequence []string, doc string, callback *janet.Function) error {
			root := c.tree
			if root == nil {
				return fmt.Errorf("missing root node")
			}

			root.Binds.Set(
				sequence,
				wm.Binding{
					Description: doc,
					Callback:    callback,
				},
			)

			return nil
		},
	)

	err = vm.Callback(
		"pane/current",
		func(context interface{}) string {
			_, ok := context.(*Client)
			if !ok {
				return ""
			}

			return "ok"
		},
	)

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
