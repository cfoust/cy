package cy

import (
	"context"
	"fmt"
	"time"

	"github.com/cfoust/cy/pkg/io/pipe"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"
	"github.com/cfoust/cy/pkg/wm"

	"github.com/sasha-s/go-deadlock"
	"github.com/xo/terminfo"
)

type Client struct {
	deadlock.RWMutex

	conn     ws.Client
	location *wm.Node
	size     wm.Size
	info     *terminfo.Terminfo
	io       pipe.Pipe[P.Message]
}

func (c *Cy) addClient(conn ws.Client, io pipe.Pipe[P.Message]) *Client {
	c.Lock()
	client := &Client{
		conn: conn,
		io:   io,
	}
	c.clients = append(c.clients, client)
	c.Unlock()

	return client
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

func (c *Client) closeError(reason error) error {
	err := c.io.Send(P.ErrorMessage{
		Message: fmt.Sprintf("error: %s", reason),
	})

	if err != nil {
		return err
	}

	return c.conn.Close()
}

func (c *Client) applyHandshake(handshake P.HandshakeMessage) error {
	c.Lock()
	defer c.Unlock()

	c.size = wm.Size{
		Rows:    handshake.Rows,
		Columns: handshake.Columns,
	}

	info, err := terminfo.Load(handshake.TERM)
	if err != nil {
		return err
	}

	c.info = info
	return nil
}

func (c *Cy) HandleClient(conn ws.Client) {
	ctx := conn.Ctx()
	io := pipe.Map[[]byte](
		conn,
		P.Encode,
		P.Decode,
	)
	events := io.Receive()

	client := c.addClient(conn, io)
	defer c.removeClient(client)

	// First we need to wait for the client's handshake to know how to
	// handle the terminal
	handshakeCtx, cancel := context.WithTimeout(ctx, 1*time.Second)
	defer cancel()

	select {
	case <-handshakeCtx.Done():
		return
	case message := <-events:
		if handshake, ok := message.Contents.(P.HandshakeMessage); ok {
			err := client.applyHandshake(handshake)
			if err != nil {
				client.closeError(err)
				return
			}
		} else {
			client.closeError(fmt.Errorf("must send handshake first"))
			return
		}
	}

	<-conn.Ctx().Done()
}

var _ ws.Server = (*Cy)(nil)
