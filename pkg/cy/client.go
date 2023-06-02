package cy

import (
	"context"
	"fmt"
	"time"

	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"
	"github.com/cfoust/cy/pkg/wm"

	"github.com/sasha-s/go-deadlock"
	"github.com/xo/terminfo"
)

type Connection ws.Client[P.Message]

type Client struct {
	deadlock.RWMutex
	conn     Connection
	location *wm.Node
	size     wm.Size
	info     *terminfo.Terminfo
}

func (c *Cy) addClient(conn Connection) *Client {
	c.Lock()
	client := &Client{
		conn: conn,
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

func (c *Cy) HandleWSClient(rawConn ws.RawClient) {
	conn := ws.MapClient[[]byte](
		rawConn,
		P.Encode,
		P.Decode,
	)

	events := conn.Receive()

	client := c.addClient(conn)
	defer c.removeClient(client)

	// First we need to wait for the client's handshake to know how to
	// handle the terminal
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

	<-conn.Ctx().Done()
}

var _ ws.Server = (*Cy)(nil)
