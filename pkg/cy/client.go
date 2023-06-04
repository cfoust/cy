package cy

import (
	"bytes"
	"context"
	"fmt"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"
	"github.com/cfoust/cy/pkg/wm"

	"github.com/sasha-s/go-deadlock"
	"github.com/xo/terminfo"
)

type Connection ws.Client[P.Message]

type Client struct {
	deadlock.RWMutex
	conn Connection
	info *terminfo.Terminfo

	location *wm.Node

	// This is a "model" of what the client is seeing so that we can
	// animate between states
	raw emu.Terminal
}

func (c *Cy) addClient(conn Connection) *Client {
	c.Lock()
	client := &Client{
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

	client.output([]byte("welcome to cy"))
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

func (c *Cy) HandleWSClient(conn ws.Client[P.Message]) {
	c.addClient(conn)
	<-conn.Ctx().Done()
}

var _ ws.Server[P.Message] = (*Cy)(nil)
