package cy

import (
	"context"
	"fmt"
	"os"
	"sync/atomic"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/cy/cmd"
	"github.com/cfoust/cy/pkg/events"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/replay"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/toasts"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/params"
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
	// The default shell
	Shell string
}

type historyEvent struct {
	Stamp  time.Time
	Client ClientID
	Node   tree.NodeID
}

type Cy struct {
	util.Lifetime
	deadlock.RWMutex
	*janet.VM

	nextClientID atomic.Int32

	muxServer *server.Server

	// The top-level fallback for all parameter queries. This is distinct
	// from the *Parameters at the root node of the tree, which the user
	// can actually change.
	defaults *params.Parameters

	// The tree of groups and panes
	tree *tree.Tree

	// Replay mode has its own isolated binding scope
	replayBinds *bind.BindScope

	clients []*Client

	log zerolog.Logger

	configPath string

	toast        *ToastLogger
	queuedToasts []toasts.Toast

	// Every time a client writes to or visits a node, we make a note of it
	// so we can infer who last used it
	// (tmux does the same thing)
	lastWrite, lastVisit map[tree.NodeID]historyEvent
	writes, visits       chan historyEvent
}

func (c *Cy) loadUserConfig(ctx context.Context) {
	err := c.ExecuteFile(ctx, c.configPath)
	if err != nil {
		c.log.Error().Err(err).Msg("failed to execute config")
		message := fmt.Sprintf("an error occurred while loading %s: %s", c.configPath, err.Error())
		c.toast.Error(message)
	}
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

func (c *Cy) pollInteractions(ctx context.Context, journal map[tree.NodeID]historyEvent, channel chan historyEvent) {
	for {
		select {
		case <-ctx.Done():
			return
		case event := <-channel:
			c.Lock()
			journal[event.Node] = event
			c.Unlock()
		}
	}
}

func (c *Cy) getClient(id ClientID) (client *Client, found bool) {
	c.RLock()
	defer c.RUnlock()

	for _, otherClient := range c.clients {
		if otherClient.id == id {
			client = otherClient
			found = true
			return
		}
	}

	return
}

func (c *Cy) inferClient(node tree.NodeID) (client *Client, found bool) {
	c.RLock()
	write, haveWrite := c.lastWrite[node]
	visit, haveVisit := c.lastVisit[node]
	c.RUnlock()

	if !haveVisit {
		return
	}

	if !haveWrite || write.Stamp.Before(visit.Stamp) {
		return c.getClient(visit.Client)
	}

	return c.getClient(write.Client)
}

func (c *Cy) pollNodeEvents(ctx context.Context, events <-chan events.Msg) {
	for {
		select {
		case <-ctx.Done():
			return
		case event := <-events:
			nodeEvent, ok := event.(tree.NodeEvent)
			if !ok {
				continue
			}

			client, ok := c.inferClient(nodeEvent.Id)
			if !ok {
				continue
			}

			switch event := nodeEvent.Event.(type) {
			case replay.CopyEvent:
				client.buffer = event.Text
			case bind.BindEvent:
				go client.runAction(event)
			}

		}
	}
}

func Start(ctx context.Context, options Options) (*Cy, error) {
	replayBinds := bind.NewBindScope()

	defaults := params.New()
	t := tree.NewTree(tree.WithParams(defaults.NewChild()))
	cy := Cy{
		Lifetime:    util.NewLifetime(ctx),
		tree:        t,
		muxServer:   server.New(),
		replayBinds: replayBinds,
		defaults:    defaults,
		lastVisit:   make(map[tree.NodeID]historyEvent),
		lastWrite:   make(map[tree.NodeID]historyEvent),
		writes:      make(chan historyEvent),
		visits:      make(chan historyEvent),
	}
	cy.toast = NewToastLogger(cy.sendToast)
	err := cy.setDefaults(options)
	if err != nil {
		return nil, err
	}

	subscriber := t.Subscribe(cy.Ctx())
	go cy.pollNodeEvents(cy.Ctx(), subscriber.Recv())
	go cy.pollInteractions(cy.Ctx(), cy.lastWrite, cy.writes)
	go cy.pollInteractions(cy.Ctx(), cy.lastVisit, cy.visits)

	if len(options.Shell) > 0 {
		replayable, _ := cmd.New(
			cy.Ctx(),
			stream.CmdOptions{
				Command: "/bin/bash",
			},
			options.DataDir,
			replayBinds,
		)

		t.Root().NewPane(cy.Ctx(), replayable)
	}

	logs := stream.NewReader()
	terminal := screen.NewTerminal(cy.Ctx(), logs, geom.DEFAULT_SIZE)
	logPane := t.Root().NewPane(cy.Ctx(), terminal)
	logPane.SetName("logs")

	consoleWriter := zerolog.ConsoleWriter{Out: logs.Writer(), TimeFormat: time.RFC3339}
	cy.log = log.Output(zerolog.MultiLevelWriter(consoleWriter, os.Stdout))

	vm, err := cy.initJanet(ctx)
	if err != nil {
		return nil, fmt.Errorf("error initializing Janet: %s", err.Error())
	}

	cy.VM = vm

	if len(options.Config) != 0 {
		cy.configPath = options.Config
		cy.loadUserConfig(ctx)
	}

	return &cy, nil
}
