package cy

import (
	"context"

	"github.com/cfoust/cy/pkg/util"
	"github.com/cfoust/cy/pkg/wm"

	"github.com/sasha-s/go-deadlock"
)

type Cy struct {
	util.Lifetime
	deadlock.RWMutex

	// The tree of groups and panes.
	tree    *wm.Node
	clients []*Client
}

func Start(ctx context.Context) (*Cy, error) {
	cy := Cy{
		Lifetime: util.NewLifetime(ctx),
	}

	pane := wm.NewPane(
		cy.Ctx(),
		wm.PaneContext{
			Command: "/bin/bash",
		},
		wm.DEFAULT_SIZE,
	)

	group := wm.NewGroup()
	group.Add(wm.Wrap("", pane))

	cy.tree = wm.Wrap("", group)

	return &cy, nil
}
