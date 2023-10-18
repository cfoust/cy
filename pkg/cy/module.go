package cy

import (
	"context"
	"os"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/util"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/sasha-s/go-deadlock"
)

type Options struct {
	// The initial Janet script, typically ~/.cyrc.janet.
	Config string
	// The default directory in which to store data (e.g. recorded sessions).
	DataDir string
}

type Cy struct {
	util.Lifetime
	deadlock.RWMutex
	janet *janet.VM

	muxServer *server.Server

	// The tree of groups and panes
	tree *tree.Tree

	// Replay mode has its own isolated binding scope
	replayBinds *bind.BindScope

	clients []*Client

	log zerolog.Logger
}

// Get the pane that new clients attach to. If there are other clients, we
// attach to the pane of the first other client. Otherwise we attach to the
// first pane we find, depth-first.
func (c *Cy) findInitialPane() tree.Node {
	c.RLock()
	defer c.RUnlock()

	if len(c.clients) > 0 {
		node := c.clients[0].Node()
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

func (c *Cy) Shutdown() error {
	c.RLock()
	defer c.RUnlock()
	for _, client := range c.clients {
		err := client.Detach("server went away")
		if err != nil {
			return err
		}
	}

	c.Cancel()

	return nil
}

func Start(ctx context.Context, options Options) (*Cy, error) {
	replayBinds := bind.NewBindScope()
	replayEvents := make(chan tree.ReplayEvent)

	t := tree.NewTree(replayBinds, replayEvents)
	cy := Cy{
		Lifetime:    util.NewLifetime(ctx),
		tree:        t,
		muxServer:   server.New(),
		replayBinds: replayBinds,
	}

	go cy.pollReplayEvents(cy.Ctx(), replayEvents)

	t.SetDataDir(options.DataDir)

	t.Root().NewCmd(
		cy.Ctx(),
		stream.CmdOptions{
			Command: "/bin/bash",
		},
		geom.DEFAULT_SIZE,
	)

	logs := stream.NewReader()
	t.Root().NewPane(
		cy.Ctx(),
		logs,
		geom.DEFAULT_SIZE,
	)

	consoleWriter := zerolog.ConsoleWriter{Out: logs.Writer(), TimeFormat: time.RFC3339}
	cy.log = log.Output(zerolog.MultiLevelWriter(consoleWriter, os.Stdout))

	vm, err := cy.initJanet(ctx)
	if err != nil {
		return nil, err
	}

	cy.janet = vm

	if len(options.Config) != 0 {
		err := vm.ExecuteFile(ctx, options.Config)
		if err != nil {
			cy.log.Error().Err(err).Str("path", options.Config).Msg("failed to execute config")
		}
	}

	return &cy, nil
}
