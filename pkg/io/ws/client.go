package ws

import (
	"context"
	"net"
	"net/http"
	"time"

	P "github.com/cfoust/cy/pkg/io/pipe"
	"github.com/cfoust/cy/pkg/util"

	"nhooyr.io/websocket"
)

type Client[T any] interface {
	Ctx() context.Context
	P.Pipe[T]
	Close() error
}

type RawClient Client[[]byte]

type WSClient struct {
	util.Lifetime
	Conn *websocket.Conn
}

const (
	WRITE_TIMEOUT = 1 * time.Second
)

func (c *WSClient) Send(data []byte) error {
	ctx, cancel := context.WithTimeout(c.Ctx(), WRITE_TIMEOUT)
	defer cancel()
	return c.Conn.Write(ctx, websocket.MessageBinary, data)
}

func (c *WSClient) Receive() <-chan P.Packet[[]byte] {
	ctx := c.Ctx()
	out := make(chan P.Packet[[]byte])
	go func() {
		for {
			if ctx.Err() != nil {
				return
			}

			typ, message, err := c.Conn.Read(ctx)
			if err != nil {
				out <- P.Packet[[]byte]{
					Error: err,
				}
				// TODO(cfoust): 05/27/23 error handling?
				c.Cancel()
				return
			}

			if typ != websocket.MessageBinary {
				continue
			}

			out <- P.Packet[[]byte]{
				Contents: message,
			}
		}
	}()

	return out
}

func (c *WSClient) Close() error {
	return c.Conn.Close(websocket.StatusNormalClosure, "")
}

var _ RawClient = (*WSClient)(nil)

func Connect(ctx context.Context, socketPath string) (RawClient, error) {
	// https://gist.github.com/teknoraver/5ffacb8757330715bcbcc90e6d46ac74
	httpClient := http.Client{
		Transport: &http.Transport{
			DialContext: func(_ context.Context, _, _ string) (net.Conn, error) {
				return net.Dial("unix", socketPath)
			},
		},
	}

	options := websocket.DialOptions{
		HTTPClient: &httpClient,
	}
	c, _, err := websocket.Dial(ctx, "http://unix/", &options)
	if err != nil {
		return nil, err
	}

	client := WSClient{
		Lifetime: util.NewLifetime(ctx),
		Conn:     c,
	}

	go func() {
		<-client.Ctx().Done()
		c.Close(websocket.StatusNormalClosure, "")
	}()

	return &client, nil
}
