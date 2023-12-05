package cy

import (
	"context"
	"fmt"
	"io"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/cy/api"
	cyParams "github.com/cfoust/cy/pkg/cy/params"
	"github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/splash"
	"github.com/cfoust/cy/pkg/mux/screen/toasts"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/mux/stream/renderer"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/rs/zerolog/log"
	"github.com/sasha-s/go-deadlock"
	"github.com/xo/terminfo"
)

type Connection = ws.Client[P.Message]

type ClientID = int32

type Client struct {
	deadlock.RWMutex
	util.Lifetime

	// the unique identifier for the client, forever
	id ClientID

	conn Connection

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

	muxClient *server.Client
	toast     *ToastLogger
	toaster   *taro.Program
	margins   *screen.Margins
	frame     *frames.Framer
	// Layers inside of the margins
	// This is for rendering content that should obey the user's margin
	// settings.
	innerLayers *screen.Layers
	// Layers outside of the margins
	outerLayers *screen.Layers
	renderer    *renderer.Renderer

	// history is an array of all of the panes this client has attached to
	history []tree.NodeID

	info screen.RenderContext
}

var _ api.Client = (*Client)(nil)

func (c *Cy) addClient(conn Connection) *Client {
	clientCtx := conn.Ctx()

	c.Lock()
	client := &Client{
		Lifetime: util.NewLifetime(clientCtx),
		cy:       c,
		conn:     conn,
		params:   params.New(),
		binds:    bind.NewEngine[bind.Action](),
	}
	c.clients = append(c.clients, client)
	c.Unlock()

	go c.pollClient(clientCtx, client)

	return client
}

func (c *Client) pollRender() {
	// TODO(cfoust): 07/16/23 replace with io.Copy
	buffer := make([]byte, 4096)

	for {
		numBytes, err := c.renderer.Read(buffer)
		if err == io.EOF {
			return
		}
		if err != nil {
			// TODO(cfoust): 07/16/23
			return
		}
		if c.Ctx().Err() != nil {
			return
		}
		if numBytes == 0 {
			continue
		}

		c.output(buffer[:numBytes])
	}
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

	log.Error().Err(err).Msgf("failed to run callback")
	c.toast.Error(fmt.Sprintf(
		"an error occurred while running %+v: %s",
		event.Sequence,
		err.Error(),
	))
}

func (c *Client) interact(out chan historyEvent) {
	c.RLock()
	node := c.node.Id()
	c.RUnlock()
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
				c.interact(c.cy.writes)
			}

			c.renderer.Send(event)
		}
	}
}

func (c *Cy) pollClient(ctx context.Context, client *Client) {
	conn := client.conn
	events := conn.Receive()

	defer c.removeClient(client)

	// First we need to wait for the client's handshake to know how to
	// handle its terminal
	handshakeCtx, cancel := context.WithTimeout(client.Ctx(), 1*time.Second)
	defer cancel()

	select {
	case <-handshakeCtx.Done():
		client.closeError(fmt.Errorf("no handshake received"))
		return
	case message, more := <-events:
		var err error
		if handshake, ok := message.Contents.(*P.HandshakeMessage); ok {
			err = client.initialize(handshake)
		} else if !more {
			err = fmt.Errorf("closed by remote")
		} else {
			err = fmt.Errorf("must send handshake first")
		}

		if err != nil {
			client.closeError(err)
			return
		}
	}

	client.id = c.nextClientID.Add(1)

	go client.pollEvents()
	go client.binds.Poll(client.Ctx())

	node := c.findInitialPane()
	if node == nil {
		// TODO(cfoust): 06/08/23 handle this
		return
	}

	client.Attach(node)

	c.sendQueuedToasts()

	c.broadcastToast(client, toasts.Toast{
		Message: "a client joined the server",
	})

	for {
		select {
		case <-conn.Ctx().Done():
			return
		case packet := <-events:
			if packet.Error != nil {
				// TODO(cfoust): 06/08/23 handle gracefully
				continue
			}

			switch packet.Contents.Type() {
			case P.MessageTypeSize:
				msg := packet.Contents.(*P.SizeMessage)
				client.Resize(geom.Vec2{
					R: msg.Rows,
					C: msg.Columns,
				})

			case P.MessageTypeInput:
				msg := packet.Contents.(*P.InputMessage)
				client.binds.Input(msg.Data)
			}
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

func (c *Client) output(data []byte) error {
	return c.conn.Send(P.OutputMessage{
		Data: data,
	})
}

func (c *Client) closeError(reason error) error {
	err := c.conn.Send(P.ErrorMessage{
		Message: fmt.Sprintf("error: %s", reason),
	})
	if err != nil {
		return err
	}

	return c.conn.Close()
}

func (c *Client) Resize(size geom.Vec2) {
	c.muxClient.Resize(size)
	c.renderer.Resize(size)
}

func isSSH(e Environment) bool {
	return e.IsSet("SSH_CONNECTION") || e.IsSet("SSH_CLIENT") || e.IsSet("SSH_TTY")
}

func (c *Client) initialize(handshake *P.HandshakeMessage) error {
	c.Lock()
	defer c.Unlock()

	c.env = Environment(handshake.Env)

	info, err := terminfo.Load(c.env.Default("TERM", "xterm-256color"))
	if err != nil {
		return err
	}

	isClientSSH := isSSH(c.env)

	c.info = screen.RenderContext{
		Terminfo: info,
		Colors:   handshake.Profile,
	}

	c.muxClient = c.cy.muxServer.AddClient(
		c.Ctx(),
		handshake.Size,
	)

	c.innerLayers = screen.NewLayers()
	c.innerLayers.NewLayer(
		c.Ctx(),
		c.muxClient,
		screen.PositionTop,
		screen.WithOpaque,
		screen.WithInteractive,
	)
	c.margins = screen.NewMargins(c.Ctx(), c.innerLayers)

	c.outerLayers = screen.NewLayers()
	c.frame = frames.NewFramer(c.Ctx(), frames.RandomFrame())
	c.outerLayers.NewLayer(
		c.Ctx(),
		c.frame,
		screen.PositionTop,
	)

	c.outerLayers.NewLayer(
		c.Ctx(),
		c.margins,
		screen.PositionTop,
		screen.WithInteractive,
		screen.WithOpaque,
	)

	splashScreen := splash.New(c.Ctx(), handshake.Size, !isClientSSH)
	c.outerLayers.NewLayer(
		splashScreen.Ctx(),
		splashScreen,
		screen.PositionTop,
		screen.WithOpaque,
		screen.WithInteractive,
	)

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
		handshake.Size,
		c.outerLayers,
	)

	if isClientSSH {
		c.params.Set(cyParams.ParamAnimate, false)
		c.toast.Send(toasts.Toast{
			Message: "you joined via SSH; disabling animation",
		})
	}

	go c.pollRender()

	return nil
}

func (c *Client) Attach(node tree.Node) error {
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
			// if the pane dies, just re-attach to the last node the user visited
			c.RLock()
			history := c.history
			c.RUnlock()

			if len(history) < 2 {
				// TODO(cfoust): 09/20/23
				return
			}

			node, ok := c.cy.tree.NodeById(history[len(history)-2])
			if !ok {
				return
			}
			c.Attach(node)
			return
		}
	}()

	c.Lock()
	c.node = node
	c.history = append(c.history, node.Id())
	c.Unlock()

	c.interact(c.cy.visits)

	// Update bindings
	scopes := make([]*bind.BindScope, 0)
	for _, pathNode := range path {
		scopes = append(scopes, pathNode.Binds())
	}

	c.binds.SetScopes(scopes...)
	c.params.SetParent(node.Params())

	return nil
}

func (c *Client) Params() *params.Parameters {
	return c.params
}

func (c *Client) Margins() *screen.Margins {
	return c.margins
}

func (c *Client) Frame() *frames.Framer {
	return c.frame
}

func (c *Client) Detach(reason string) error {
	err := c.conn.Send(P.CloseMessage{
		Reason: reason,
	})
	if err != nil {
		return err
	}

	return c.conn.Close()
}

func (c *Cy) HandleWSClient(conn ws.Client[P.Message]) {
	c.addClient(conn)
	<-conn.Ctx().Done()
}

var _ ws.Server[P.Message] = (*Cy)(nil)
