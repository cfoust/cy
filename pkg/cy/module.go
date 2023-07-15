package cy

import (
	"context"
	"os"
	"time"

	"github.com/cfoust/cy/pkg/ui/io"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/util"
	"github.com/cfoust/cy/pkg/wm"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/sasha-s/go-deadlock"
)

type Cy struct {
	util.Lifetime
	deadlock.RWMutex
	janet *janet.VM

	// The tree of groups and panes.
	tree    *wm.Tree
	clients []*Client

	log zerolog.Logger
}

// Get the pane that new clients attach to. If there are other clients, we
// attach to the pane of the first other client. Otherwise we attach to the
// first pane we find, depth-first.
func (c *Cy) findInitialPane() wm.Node {
	c.RLock()
	defer c.RUnlock()

	if len(c.clients) > 0 {
		node := c.clients[0].GetNode()
		if node != nil {
			return node
		}
	}

	leaves := c.tree.Leaves()
	if len(leaves) == 0 {
		return nil
	}

	return leaves[0]
}

// Given a node, get a list of all clients attached to it and find the minimum
// pane size.
func (c *Cy) refreshPane(node wm.Node) {
	pane, ok := node.(*wm.Pane)
	if !ok {
		return
	}

	c.Lock()
	defer c.Unlock()

	// Get a list of all clients attached to this node
	attached := make([]*Client, 0)
	for _, client := range c.clients {
		if node == client.GetNode() {
			attached = append(attached, client)
		}
	}

	// Don't do anything if no clients are attached to this pane
	if len(attached) == 0 {
		return
	}

	// Set the pane's size to the maximum that all clients can fit
	size := attached[0].GetSize()
	for _, client := range attached {
		size = geom.GetMaximum(size, client.GetSize())
	}

	pane.Resize(size)
}

func Start(ctx context.Context, configFile string) (*Cy, error) {
	tree := wm.NewTree()

	cy := Cy{
		Lifetime: util.NewLifetime(ctx),
		tree:     tree,
	}

	tree.Root().NewCmd(
		cy.Ctx(),
		io.CmdOptions{
			Command: "/bin/bash",
		},
		geom.DEFAULT_SIZE,
	)

	logs := io.NewStream()
	tree.Root().NewPane(
		cy.Ctx(),
		logs,
		geom.DEFAULT_SIZE,
	)

	consoleWriter := zerolog.ConsoleWriter{Out: logs.Writer(), TimeFormat: time.RFC3339}
	cy.log = log.Output(zerolog.MultiLevelWriter(consoleWriter, os.Stdout))

	vm, err := cy.initJanet(ctx, configFile)
	if err != nil {
		return nil, err
	}

	cy.janet = vm

	return &cy, nil
}
