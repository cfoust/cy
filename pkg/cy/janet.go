package cy

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/cy/api"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/util"
)

import _ "embed"

//go:embed cy-boot.janet
var CY_BOOT_FILE []byte

func (c *Cy) initJanet(ctx context.Context) (*janet.VM, error) {
	vm, err := janet.New(ctx)
	if err != nil {
		return nil, err
	}

	modules := map[string]interface{}{
		"cmd": &api.CmdModule{
			Lifetime:  util.NewLifetime(c.Ctx()),
			Tree:      c.tree,
			TimeBinds: c.timeBinds,
			CopyBinds: c.copyBinds,
		},
		"cy":     &CyModule{cy: c},
		"exec":   &api.ExecModule{Server: c},
		"group":  &api.GroupModule{Tree: c.tree},
		"input":  &api.InputModule{Tree: c.tree, Server: c.muxServer},
		"layout": &api.LayoutModule{},
		"msg":    &api.MsgModule{Server: c},
		"key": &api.KeyModule{
			Tree:      c.tree,
			TimeBinds: c.timeBinds,
			CopyBinds: c.copyBinds,
		},
		"pane":  &api.PaneModule{Tree: c.tree},
		"param": &api.ParamModule{Tree: c.tree},
		"path":  &api.PathModule{},
		"replay": &api.ReplayModule{
			Lifetime:  util.NewLifetime(c.Ctx()),
			Tree:      c.tree,
			TimeBinds: c.timeBinds,
			CopyBinds: c.copyBinds,
		},
		"tree":     &api.TreeModule{Tree: c.tree},
		"viewport": &api.ViewportModule{},
	}

	for name, module := range modules {
		err := vm.Module(name, module)
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
		return nil, fmt.Errorf("failed to execute cy-boot.janet: %s", err.Error())
	}

	return vm, nil
}
