package cy

import (
	"context"
	"fmt"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/cy/api"
	"github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/splash"
	"github.com/cfoust/cy/pkg/mux/screen/toasts"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/mux/stream/renderer"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
	"github.com/xo/terminfo"
)

type ClientID = int32

type ClientOptions = P.HandshakeMessage

type Client struct {
	deadlock.RWMutex
	util.Lifetime

	// the unique identifier for the client, forever
	id ClientID

	cy *Cy

	// All of the environment variables in the client's original
	// environment at connection time
	env Environment

	node  tree.Node
	binds *bind.Engine[bind.Action]

	// the text the client has copied
	buffer string

	// the client can have params of their own
	params *params.Parameters

	muxClient    *server.Client
	layoutEngine *layout.LayoutEngine
	toast        *ToastLogger
	toaster      *taro.Program
	frame        *frames.Framer
	outerLayers  *screen.Layers
	renderer     *renderer.Renderer

	// An array of all of the panes this client has attached to.
	history []tree.NodeID
	// The client's current location in history.
	historyIndex int
}

var _ api.Client = (*Client)(nil)

var _ mux.Stream = (*Client)(nil)

func (c *Cy) NewClient(ctx context.Context, options ClientOptions) (*Client, error) {
	client := &Client{
		Lifetime: util.NewLifetime(ctx),
		cy:       c,
		params:   params.New(),
		binds:    bind.NewEngine[bind.Action](),
	}

	err := client.initialize(options)
	if err != nil {
		return nil, err
	}

	c.Lock()
	c.clients = append(c.clients, client)
	c.Unlock()

	client.id = c.nextClientID.Add(1)

	go client.pollEvents()
	go client.binds.Poll(client.Ctx())

	err = client.findNewPane()
	if err != nil {
		return nil, err
	}

	go func() {
		select {
		case <-c.Ctx().Done():
		case <-client.Ctx().Done():
		}

		c.removeClient(client)
	}()

	c.sendQueuedToasts()
	c.broadcastToast(client, toasts.Toast{
		Message: "a client joined the server",
	})

	return client, nil
}

func (c *Client) Kill() {
	c.renderer.Kill()
}

func (c *Client) Read(p []byte) (n int, err error) {
	return c.renderer.Read(p)
}

func (c *Client) Write(data []byte) (n int, err error) {
	c.binds.Input(data)
	return len(data), nil
}

func (c *Client) Resize(size geom.Vec2) error {
	c.muxClient.Resize(size)
	c.renderer.Resize(size)
	return nil
}

func (c *Client) runAction(event bind.BindEvent) {
	args := make([]interface{}, 0)
	for _, arg := range event.Args {
		args = append(args, arg)
	}

	err := event.Action.Callback.CallContext(
		c.Ctx(),
		c,
		args...,
	)
	if err == nil || err == context.Canceled {
		return
	}

	msg := fmt.Sprintf(
		"an error occurred while running %+v: %s",
		event.Sequence,
		err.Error(),
	)

	c.cy.log.Error().Msg(msg)
	c.toast.Error(msg)
}

func (c *Client) interact(out chan historyEvent, node tree.NodeID) {
	out <- historyEvent{
		Client: c.id,
		Node:   node,
		Stamp:  time.Now(),
	}
}

func (c *Client) pollEvents() {
	for {
		select {
		case <-c.Ctx().Done():
			return
		case event := <-c.binds.Recv():
			if bind, ok := event.(bind.BindEvent); ok {
				go c.runAction(bind)
				continue
			}

			// We only consider key presses to be an interaction
			// We don't want mouse motion to trigger this
			if _, ok := event.(taro.KeyMsg); ok {
				c.RLock()
				node := c.node.Id()
				c.RUnlock()
				c.interact(c.cy.writes, node)
			}

			c.renderer.Send(event)
		}
	}
}

func (c *Client) Node() tree.Node {
	c.RLock()
	defer c.RUnlock()
	return c.node
}

func (c *Client) OuterLayers() *screen.Layers {
	return c.outerLayers
}

func (c *Client) Layout() *layout.LayoutEngine {
	return c.layoutEngine
}

func (c *Cy) removeClient(client *Client) {
	c.Lock()
	newClients := make([]*Client, 0)
	for _, other := range c.clients {
		if client == other {
			continue
		}

		newClients = append(newClients, other)
	}
	c.clients = newClients
	c.Unlock()
}

func isSSH(e Environment) bool {
	return e.IsSet("SSH_CONNECTION") || e.IsSet("SSH_CLIENT") || e.IsSet("SSH_TTY")
}

func (c *Client) initialize(options ClientOptions) error {
	c.Lock()
	defer c.Unlock()

	c.env = Environment(options.Env)

	info, err := terminfo.Load(c.env.Default("TERM", "xterm-256color"))
	if err != nil {
		return err
	}

	isClientSSH := isSSH(c.env)

	c.muxClient = c.cy.muxServer.AddClient(c.Ctx(), options.Size)

	c.layoutEngine = layout.NewLayoutEngine(c.Ctx())

	c.outerLayers = screen.NewLayers()

	frame := frames.RandomFrame()
	defaultFrame := c.cy.tree.Root().Params().DefaultFrame()
	if len(defaultFrame) != 0 {
		if newFrame, ok := frames.Frames[defaultFrame]; ok {
			frame = newFrame
		}
	}
	c.frame = frames.NewFramer(c.Ctx(), frame)

	c.outerLayers.NewLayer(
		c.Ctx(),
		c.frame,
		screen.PositionTop,
	)

	c.outerLayers.NewLayer(
		c.Ctx(),
		c.layoutEngine,
		screen.PositionTop,
		screen.WithInteractive,
	)

	if c.cy.showSplash {
		splashScreen := splash.New(c.Ctx(), options.Size, !isClientSSH)
		c.outerLayers.NewLayer(
			splashScreen.Ctx(),
			splashScreen,
			screen.PositionTop,
			screen.WithOpaque,
			screen.WithInteractive,
		)
	}

	c.toaster = toasts.New(c.Ctx())
	c.toast = NewToastLogger(c.sendToast)
	c.outerLayers.NewLayer(
		c.Ctx(),
		c.toaster,
		screen.PositionTop,
	)

	c.renderer = renderer.NewRenderer(
		c.Ctx(),
		info,
		options.Size,
		c.outerLayers,
	)

	if isClientSSH {
		c.params.SetAnimate(false)
		c.toast.Send(toasts.Toast{
			Message: "you joined via SSH; disabling animation",
		})
	}

	return nil
}

// findNewPane looks for a pane that the client can attach to or creates a new
// one if none are suitable.
func (c *Client) findNewPane() error {
	c.RLock()
	history := c.history
	c.RUnlock()

	// First look back in history
	for i := len(history) - 2; i >= 0; i-- {
		node, ok := c.cy.tree.NodeById(history[i])
		if !ok {
			continue
		}
		return c.Attach(node)
	}

	// Then see if there are any other client panes
	clientNode := c.cy.getFirstClientPane(c)
	if clientNode != nil {
		return c.Attach(clientNode)
	}

	// Otherwise just create a new shell and attach to it
	return c.execute(`(shell/attach)`)
}

func (c *Client) attach(node tree.Node) error {
	pane, ok := node.(*tree.Pane)
	if !ok {
		return fmt.Errorf("node was not a pane")
	}

	path := c.cy.tree.PathTo(node)
	if len(path) == 0 {
		return fmt.Errorf("failed to find path to node")
	}

	c.muxClient.Attach(c.Ctx(), pane.Screen())

	go func() {
		select {
		case <-c.Ctx().Done():
			return
		case <-c.muxClient.Attachment().Ctx().Done():
			return
		case <-pane.Ctx().Done():
			// TODO(cfoust): 12/15/23 handle error
			c.findNewPane()
			return
		}
	}()

	c.node = node

	// Update bindings
	scopes := make([]*bind.BindScope, 0)
	for _, pathNode := range path {
		scopes = append(scopes, pathNode.Binds())
	}

	c.binds.SetScopes(scopes...)
	c.params.SetParent(node.Params())
	c.interact(c.cy.visits, node.Id())
	return nil
}

func (c *Client) Attach(node tree.Node) error {
	c.Lock()
	defer c.Unlock()

	err := c.attach(node)
	if err != nil {
		return err
	}

	// If we're back in time, start the history index anew
	if c.historyIndex < len(c.history)-1 {
		c.history = c.history[:c.historyIndex+1]
	}

	c.history = append(c.history, node.Id())
	c.historyIndex = len(c.history) - 1
	return nil
}

func (c *Client) HistoryForward() error {
	c.Lock()
	defer c.Unlock()

	var (
		history      = c.history
		historyIndex = c.historyIndex
	)

	for i := historyIndex + 1; i < len(history); i++ {
		node, ok := c.cy.tree.NodeById(history[i])
		if !ok {
			continue
		}
		err := c.attach(node)
		if err != nil {
			return err
		}

		c.historyIndex = i
		break
	}

	return nil
}

func (c *Client) HistoryBackward() error {
	c.Lock()
	defer c.Unlock()

	var (
		history      = c.history
		historyIndex = c.historyIndex
	)

	for i := historyIndex - 1; i >= 0; i-- {
		node, ok := c.cy.tree.NodeById(history[i])
		if !ok {
			continue
		}

		err := c.attach(node)
		if err != nil {
			return err
		}

		c.historyIndex = i
		break
	}

	return nil
}

func (c *Client) Get(key string) (value interface{}, ok bool) {
	return c.params.Get(key)
}

func (c *Client) Params() *params.Parameters {
	return c.params
}

func (c *Client) Frame() *frames.Framer {
	return c.frame
}

func (c *Client) Binds() (binds []api.Binding) {
	for _, scope := range c.binds.Scopes() {
		for _, leaf := range scope.Leaves() {
			binds = append(
				binds,
				api.Binding{
					Sequence: leaf.Path,
					Tag:      leaf.Value.Tag,
					Function: leaf.Value.Callback.Value,
				},
			)
		}
	}
	return
}

func (c *Client) Detach() {
	c.Cancel()
}

// execute runs some Janet code on behalf of the client.
func (c *Client) execute(code string) error {
	return c.cy.ExecuteCall(c.Ctx(), c, janet.Call{
		Code:    []byte(code),
		Options: janet.DEFAULT_CALL_OPTIONS,
	})
}

func (c *Client) Toast(toast toasts.Toast) {
	c.toaster.Send(toast)
}
