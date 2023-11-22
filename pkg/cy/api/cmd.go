package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/cy/cmd"
	"github.com/cfoust/cy/pkg/cy/params"
	cyParams "github.com/cfoust/cy/pkg/cy/params"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/replayable"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/util"
)

type CmdParams struct {
	Command string
	Args    []string
	Name    string
}

type Cmd struct {
	Lifetime    util.Lifetime
	Tree        *tree.Tree
	ReplayBinds *bind.BindScope
}

func (c *Cmd) New(
	user interface{},
	groupId tree.NodeID,
	path string,
	cmdParams *janet.Named[CmdParams],
) (tree.NodeID, error) {
	client, ok := user.(Client)
	if !ok {
		return 0, fmt.Errorf("missing client context")
	}

	shell := "/bin/bash"
	defaultShell, ok := client.Params().Get(cyParams.ParamDefaultShell)
	if value, ok := defaultShell.(string); ok {
		shell = value
	}

	values := cmdParams.WithDefault(CmdParams{
		Command: shell,
	})

	group, ok := c.Tree.GroupById(groupId)
	if !ok {
		return 0, fmt.Errorf("node not found: %d", groupId)
	}

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
			Directory: path,
		},
		dataDir,
		c.ReplayBinds,
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

func (c *Cmd) Path(id tree.NodeID) (*string, error) {
	pane, ok := c.Tree.PaneById(id)
	if !ok {
		return nil, fmt.Errorf("pane not found: %d", id)
	}

	r, ok := pane.Screen().(*replayable.Replayable)
	if !ok {
		return nil, fmt.Errorf("pane was not a cmd")
	}

	cmd, ok := r.Stream().(*stream.Cmd)
	if !ok {
		return nil, fmt.Errorf("pane was not a cmd")
	}

	path, err := cmd.Path()
	if err != nil {
		return nil, err
	}

	return &path, nil
}
