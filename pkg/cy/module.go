package cy

import (
	"context"
	"fmt"
	"os"
	"sync/atomic"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/events"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/toasts"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/replay"
	"github.com/cfoust/cy/pkg/util"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
	"github.com/sasha-s/go-deadlock"
)

type Options struct {
	// The initial Janet script, typically ~/.cyrc.janet.
	Config string
	// The default directory in which to store data (e.g. recorded
	// sessions).
	DataDir string
	// The default shell
	Shell string
	// Whether to show the splash screen on client join
	HideSplash bool
	// Whether to skip blocking (input/*) API calls. Just for testing.
	SkipInput bool
	// The path to the Unix domain socket for this server.
	SocketPath string
	// The name of the socket (before calculating the real path.)
	SocketName string
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
	timeBinds *bind.BindScope
	// So does copy mode
	copyBinds *bind.BindScope

	clients []*Client

	log zerolog.Logger

	options Options

	toast        *ToastLogger
	queuedToasts []toasts.Toast

	// Every time a client writes to or visits a node, we make a note of it
	// so we can infer who last used it
	// (tmux does the same thing)
	lastWrite, lastVisit map[tree.NodeID]historyEvent
	writes, visits       chan historyEvent
}

func (c *Cy) ExecuteJanet(path string) error {
	return c.ExecuteFile(c.Ctx(), path)
}

func (c *Cy) Log(level zerolog.Level, message string) {
	c.log.WithLevel(level).Msgf(message)
}

func (c *Cy) loadConfig() error {
	err := c.ExecuteFile(c.Ctx(), c.options.Config)

	// We want to make a lot of noise if this fails for some reason, even
	// if this is being called in user code
	if err != nil {
		c.log.Error().Err(err).Msg("failed to execute config")
		message := fmt.Sprintf(
			"an error occurred while loading %s: %s",
			c.options.Config,
			err.Error(),
		)
		c.toast.Error(message)
	}

	return err
}

func (c *Cy) reloadConfig() error {
	path := FindConfig()
	if len(path) == 0 {
		return nil
	}

	c.Lock()
	c.options.Config = path
	c.Unlock()

	return c.loadConfig()
}

// Get the first pane that another client is attached to or return nil if there
// are no other clients.
func (c *Cy) getFirstClientPane(except *Client) tree.Node {
	c.RLock()
	defer c.RUnlock()

	clients := c.clients

	if len(clients) == 0 {
		return nil
	}

	for _, client := range clients {
		if client == except {
			continue
		}

		node := client.Node()
		if node != nil {
			return node
		}
	}

	return nil
}

func (c *Cy) Shutdown() error {
	c.RLock()
	defer c.RUnlock()
	for _, client := range c.clients {
		client.Detach()
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

func (c *Cy) SocketName() string {
	return c.options.SocketName
}

func Start(ctx context.Context, options Options) (*Cy, error) {
	timeBinds := bind.NewBindScope(nil)
	copyBinds := bind.NewBindScope(nil)

	defaults := params.New()
	t := tree.NewTree(tree.WithParams(defaults.NewChild()))
	cy := Cy{
		Lifetime:  util.NewLifetime(ctx),
		tree:      t,
		muxServer: server.New(),
		defaults:  defaults,
		timeBinds: timeBinds,
		copyBinds: copyBinds,
		options:   options,
		lastVisit: make(map[tree.NodeID]historyEvent),
		lastWrite: make(map[tree.NodeID]historyEvent),
		writes:    make(chan historyEvent),
		visits:    make(chan historyEvent),
	}
	cy.toast = NewToastLogger(cy.sendToast)

	// Some parameter defaults are set at runtime
	for key, value := range map[string]interface{}{
		params.ParamDataDirectory: options.DataDir,
		params.ParamDefaultShell:  options.Shell,
		params.ParamSkipInput:     options.SkipInput,
	} {
		err := defaults.Set(key, value)
		if err != nil {
			return nil, err
		}
	}

	subscriber := t.Subscribe(cy.Ctx())
	go cy.pollNodeEvents(cy.Ctx(), subscriber.Recv())
	go cy.pollInteractions(cy.Ctx(), cy.lastWrite, cy.writes)
	go cy.pollInteractions(cy.Ctx(), cy.lastVisit, cy.visits)

	logs := stream.NewReader()
	logScreen := replay.NewReplayable(
		cy.Ctx(),
		logs,
		logs,
		timeBinds,
		copyBinds,
	)
	logPane := t.Root().NewPane(cy.Ctx(), logScreen)
	logPane.SetName("logs")
	logPane.SetProtected(true)
	logs.Write([]byte(emu.LineFeedMode))

	consoleWriter := zerolog.ConsoleWriter{Out: logs.Writer(), TimeFormat: time.RFC3339}
	cy.log = log.Output(zerolog.MultiLevelWriter(consoleWriter, os.Stdout))

	vm, err := cy.initJanet(ctx)
	if err != nil {
		return nil, fmt.Errorf("error initializing Janet: %s", err.Error())
	}

	cy.VM = vm

	if len(options.Config) != 0 {
		cy.loadConfig()
	}

	return &cy, nil
}
