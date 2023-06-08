package cy

import (
	"bytes"
	"context"
	"fmt"
	"time"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/emu"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"
	"github.com/cfoust/cy/pkg/util"
	"github.com/cfoust/cy/pkg/wm"

	"github.com/rs/zerolog/log"
	"github.com/sasha-s/go-deadlock"
	"github.com/xo/terminfo"
)

type Connection ws.Client[P.Message]

type Client struct {
	deadlock.RWMutex
	conn Connection

	cy *Cy

	info *terminfo.Terminfo

	attachment *util.Lifetime
	node       *wm.Node
	updates    *util.Subscriber[time.Time]

	// This is a "model" of what the client is seeing so that we can
	// animate between states
	raw emu.Terminal
}

func (c *Cy) addClient(conn Connection) *Client {
	c.Lock()
	client := &Client{
		cy: c,
		conn: conn,
	}
	c.clients = append(c.clients, client)
	c.Unlock()

	go c.pollClient(client)

	return client
}

func (c *Cy) pollClient(client *Client) {
	conn := client.conn
	events := conn.Receive()

	defer c.removeClient(client)

	// First we need to wait for the client's handshake to know how to
	// handle its terminal
	handshakeCtx, cancel := context.WithTimeout(conn.Ctx(), 1*time.Second)
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

	buf := new(bytes.Buffer)
	client.info.Fprintf(buf, terminfo.ClearScreen)
	client.info.Fprintf(buf, terminfo.CursorHome)
	client.output(buf.Bytes())

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
			case P.MessageTypeInput:
				msg := packet.Contents.(*P.InputMessage)
				node := client.GetNode()
				if node == nil {
					continue
				}

				log.Info().Msgf("passing input %+v", msg.Data)
				pane := node.Data.(*wm.Pane)
				pane.Write(msg.Data)
			}
		}
	}
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

func (c *Client) initialize(handshake *P.HandshakeMessage) error {
	c.Lock()
	defer c.Unlock()

	info, err := terminfo.Load(handshake.TERM)
	if err != nil {
		return err
	}
	c.info = info

	c.raw = emu.New(
		emu.WithSize(
			handshake.Columns,
			handshake.Rows,
		),
	)

	return nil
}

func (c *Client) GetNode() *wm.Node {
	c.RLock()
	node := c.node
	c.RUnlock()

	return node
}

func (c *Client) GetSize() wm.Size {
	c.raw.Lock()
	cols, rows := c.raw.Size()
	c.raw.Unlock()

	return wm.Size{
		Rows:    rows,
		Columns: cols,
	}
}

func (c *Client) pollPane(ctx context.Context, pane *wm.Pane) error {
	log.Info().Msgf("polling pane %+v", pane)
	defer log.Info().Msgf("done polling pane %+v", pane)

	subscriber := pane.Subscribe()
	defer subscriber.Done()

	changes := subscriber.Recv()

	for {
		c.output(anim.SwapView(
			c.info,
			pane.Terminal,
			c.raw,
		))

		select {
		case <-ctx.Done():
			return nil
		case <-changes:
			continue
		}
	}
}

func (c *Client) Attach(node *wm.Node) error {
	pane, ok := node.Data.(*wm.Pane)
	if !ok {
		return fmt.Errorf("node was not a pane")
	}

	attachment := util.NewLifetime(c.conn.Ctx())

	c.Lock()
	if c.attachment != nil {
		c.attachment.Cancel()
	}
	c.attachment = &attachment

	oldNode := c.node
	c.node = node
	c.Unlock()

	if oldNode != nil {
		c.cy.refreshPane(oldNode)
	}
	c.cy.refreshPane(node)

	go c.pollPane(attachment.Ctx(), pane)

	return nil
}

func (c *Cy) HandleWSClient(conn ws.Client[P.Message]) {
	c.addClient(conn)
	<-conn.Ctx().Done()
}

var _ ws.Server[P.Message] = (*Cy)(nil)
