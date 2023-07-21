package cy

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"time"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/fuzzy"
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/util"

	"github.com/muesli/termenv"
	"github.com/rs/zerolog/log"
	"github.com/sasha-s/go-deadlock"
	"github.com/xo/terminfo"
)

type Connection = ws.Client[P.Message]

type Client struct {
	deadlock.RWMutex
	util.Lifetime

	conn Connection

	cy *Cy

	node  tree.Node
	binds *bind.Engine[tree.Binding]

	muxClient *server.Client
	layers    *screen.Layers
	renderer  *stream.Renderer

	raw          emu.Terminal
	info         *terminfo.Terminfo
	colorProfile termenv.Profile
}

func (c *Cy) addClient(conn Connection) *Client {
	clientCtx := conn.Ctx()

	c.Lock()
	client := &Client{
		Lifetime: util.NewLifetime(clientCtx),
		cy:       c,
		conn:     conn,
		binds:    bind.NewEngine[tree.Binding](),
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

func (c *Client) pollEvents() {
	for {
		select {
		case <-c.Ctx().Done():
			return
		case event := <-c.binds.Recv():
			switch event := event.(type) {
			case bind.ActionEvent[tree.Binding]:
				err := event.Action.Callback.CallContext(
					c.Ctx(),
					c,
				)
				if err != nil {
					log.Error().Err(err).Msgf("failed to run callback")
				}
				continue
			case bind.RawEvent:
				// TODO(cfoust): 07/18/23 error handling
				c.renderer.Write(event.Data)
			}
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

	go client.pollEvents()
	go client.binds.Poll(client.Ctx())

	client.clearScreen()

	node := c.findInitialPane()
	if node == nil {
		// TODO(cfoust): 06/08/23 handle this
		return
	}

	client.Attach(node)

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

func (c *Client) clearScreen() {
	buf := new(bytes.Buffer)
	c.info.Fprintf(buf, terminfo.ClearScreen)
	c.info.Fprintf(buf, terminfo.CursorHome)
	c.output(buf.Bytes())
}

func (c *Client) output(data []byte) error {
	_, err := c.raw.Write(data)
	if err != nil {
		return err
	}

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

// Trigger fuzzy finding.
func (c *Client) Find(choices []string) {
	fuzzy := fuzzy.NewFuzzy(
		c.Ctx(),
		c.colorProfile,
		c.info,
		choices,
	)

	c.layers.NewLayer(
		fuzzy.Ctx(),
		fuzzy,
		true,
	)
}

func (c *Client) initialize(handshake *P.HandshakeMessage) error {
	c.Lock()
	defer c.Unlock()

	info, err := terminfo.Load(handshake.TERM)
	if err != nil {
		return err
	}
	c.info = info
	c.colorProfile = handshake.Profile

	c.muxClient = c.cy.muxServer.AddClient(
		c.Ctx(),
		info,
		handshake.Size,
	)

	c.layers = screen.NewLayers()
	c.layers.NewLayer(
		c.Ctx(),
		c.muxClient,
		true,
	)

	c.raw = emu.New(emu.WithSize(handshake.Size))
	c.renderer = stream.NewRenderer(c.Ctx(), info, c.raw, c.layers)

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

	c.Lock()
	c.node = node
	c.Unlock()

	// Update bindings
	scopes := make([]*tree.BindScope, 0)
	for _, pathNode := range path {
		scopes = append(scopes, pathNode.Binds())
	}
	c.binds.SetScopes(scopes...)

	return nil
}

func (c *Cy) HandleWSClient(conn ws.Client[P.Message]) {
	c.addClient(conn)
	<-conn.Ctx().Done()
}

var _ ws.Server[P.Message] = (*Cy)(nil)
