package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/cy/cmd"
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
	DataDir     string
	replayBinds *bind.BindScope
}

func (c *Cmd) New(
	groupId tree.NodeID,
	path string,
	params *janet.Named[CmdParams],
) (tree.NodeID, error) {
	values := params.WithDefault(CmdParams{
		Command: "/bin/bash",
	})

	group, ok := c.Tree.GroupById(groupId)
	if !ok {
		return 0, fmt.Errorf("node not found: %d", groupId)
	}

	replayable, err := cmd.New(
		c.Lifetime.Ctx(),
		stream.CmdOptions{
			Command:   values.Command,
			Args:      values.Args,
			Directory: path,
		},
		c.DataDir,
		c.replayBinds,
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
		return nil, fmt.Errorf("pane was not a replayable")
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
