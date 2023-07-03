package cy

import (
	"context"

	"github.com/cfoust/cy/pkg/janet"
)

type JanetCall interface{}

func (c *Cy) initJanet(ctx context.Context, configFile string) (*janet.VM, error) {
	vm, err := janet.New(ctx)
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
