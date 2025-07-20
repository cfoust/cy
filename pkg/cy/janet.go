package cy

import (
	"context"
	"embed"
	"fmt"

	"github.com/cfoust/cy/pkg/cy/api"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/util"
)

//go:embed boot/*.janet
var CY_BOOT embed.FS

func (c *Cy) initJanet(ctx context.Context) (*janet.VM, error) {
	vm, err := janet.New(ctx)
	if err != nil {
		return nil, err
	}

	modules := map[string]interface{}{
		"register": &api.RegisterModule{
			Registers: c.registers,
		},
		"cmd": &api.CmdModule{
			Server:    c,
			Lifetime:  util.NewLifetime(c.Ctx()),
			Tree:      c.tree,
			TimeBinds: c.timeBinds,
			CopyBinds: c.copyBinds,
			Store:     c.cmdStore,
		},
		"color-maps": &api.ColorMapsModule{},
		"cy":         &CyModule{cy: c},
		"exec":       &api.ExecModule{Server: c},
		"group":      &api.GroupModule{Tree: c.tree},
		"input":      &api.InputModule{Tree: c.tree, Server: c.muxServer},
		"layout":     &api.LayoutModule{},
		"msg":        &api.MsgModule{Server: c},
		"key": &api.KeyModule{
			Tree:        c.tree,
			SearchBinds: c.searchBinds,
			TimeBinds:   c.timeBinds,
			CopyBinds:   c.copyBinds,
		},
		"pane": &api.PaneModule{Tree: c.tree},
		"param": &api.ParamModule{
			Server: c,
			Tree:   c.tree,
		},
		"path": &api.PathModule{},
		"replay": &api.ReplayModule{
			Lifetime:  util.NewLifetime(c.Ctx()),
			Tree:      c.tree,
			TimeBinds: c.timeBinds,
			CopyBinds: c.copyBinds,
		},
		"search": &api.SearchModule{
			Lifetime:    util.NewLifetime(c.Ctx()),
			Tree:        c.tree,
			SearchBinds: c.searchBinds,
			TimeBinds:   c.timeBinds,
			CopyBinds:   c.copyBinds,
		},
		"style":    &api.StyleModule{},
		"time":     &api.TimeModule{},
		"tree":     &api.TreeModule{Tree: c.tree},
		"viewport": &api.ViewportModule{},
	}

	for name, module := range modules {
		err := vm.Module(name, module)
		if err != nil {
			return nil, err
		}
	}

	// These are specified here because order matters and I think something
	// like 01_actions.janet, 02_layout.janet is ugly
	files := []string{
		"time.janet",
		"input.janet",
		"actions.janet",
		"style.janet",
		"colors.janet",
		"layout.janet",
		"register.janet",
		"replay.janet",
		"binds.janet",
	}

	for _, file := range files {
		path := "boot/" + file

		data, err := CY_BOOT.ReadFile(path)
		if err != nil {
			return nil, err
		}

		_, err = vm.ExecuteCall(ctx, nil, janet.Call{
			Code:       data,
			SourcePath: path,
			Options:    janet.DEFAULT_CALL_OPTIONS,
		})
		if err != nil {
			return nil, fmt.Errorf(
				"failed to execute %s: %s",
				file,
				err.Error(),
			)
		}
	}

	return vm, nil
}
