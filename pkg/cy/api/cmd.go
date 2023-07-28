package api

import (
	"fmt"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/janet"
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
	Lifetime util.Lifetime
	Tree     *tree.Tree
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

	cmd, err := group.NewCmd(
		c.Lifetime.Ctx(),
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

	if values.Name != "" {
		cmd.SetName(values.Name)
	}

	return cmd.Id(), nil
}

func (c *Cmd) Path(id tree.NodeID) (*string, error) {
	pane, ok := c.Tree.PaneById(id)
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
}
