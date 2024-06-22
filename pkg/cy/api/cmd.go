package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/cy/cmd"
	"github.com/cfoust/cy/pkg/cy/params"
	cyParams "github.com/cfoust/cy/pkg/cy/params"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/replay"
	"github.com/cfoust/cy/pkg/replay/detect"
	"github.com/cfoust/cy/pkg/util"
)

type CmdParams struct {
	Command string
	Args    []string
	Name    string
	Path    string
}

type CmdModule struct {
	Lifetime             util.Lifetime
	Tree                 *tree.Tree
	TimeBinds, CopyBinds *bind.BindScope
}

func (c *CmdModule) New(
	user interface{},
	groupId *janet.Value,
	cmdParams *janet.Named[CmdParams],
) (tree.NodeID, error) {
	defer groupId.Free()

	group, err := resolveGroup(c.Tree, groupId)
	if err != nil {
		return 0, err
	}

	command := "/bin/bash"
	if client, ok := user.(Client); ok {
		defaultShell, _ := client.Get(cyParams.ParamDefaultShell)
		if value, ok := defaultShell.(string); ok {
			command = value
		}
	}

	values := cmdParams.WithDefault(CmdParams{
		Command: command,
	})

	param, _ := group.Params().Get(params.ParamDataDirectory)
	dataDir, ok := param.(string)
	if !ok {
		return 0, fmt.Errorf("param %s was not a string", params.ParamDataDirectory)
	}

	replayable, err := cmd.New(
		c.Lifetime.Ctx(),
		stream.CmdOptions{
			Command:   values.Command,
			Args:      values.Args,
			Directory: values.Path,
		},
		dataDir,
		c.TimeBinds,
		c.CopyBinds,
	)
	if err != nil {
		return 0, err
	}

	pane := group.NewPane(c.Lifetime.Ctx(), replayable)
	if values.Name != "" {
		pane.SetName(values.Name)
	}

	return pane.Id(), nil
}

func (c *CmdModule) Path(id *janet.Value) (*string, error) {
	defer id.Free()

	pane, err := resolvePane(c.Tree, id)
	if err != nil {
		return nil, err
	}

	r, ok := pane.Screen().(*replay.Replayable)
	if !ok {
		return nil, fmt.Errorf("pane was not a cmd")
	}

	cmd, ok := r.Cmd().(*stream.Cmd)
	if !ok {
		return nil, fmt.Errorf("pane was not a cmd")
	}

	path, err := cmd.Path()
	if err != nil {
		return nil, err
	}

	return &path, nil
}

func (c *CmdModule) Commands(id *janet.Value) (*[]detect.Command, error) {
	defer id.Free()

	pane, err := resolvePane(c.Tree, id)
	if err != nil {
		return nil, err
	}

	r, ok := pane.Screen().(*replay.Replayable)
	if !ok {
		return nil, fmt.Errorf("pane was not a cmd")
	}

	commands := r.Commands()
	return &commands, nil
}
