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

type Protocol[T any] interface {
	Encode(data T) ([]byte, error)
	Decode(data []byte) (T, error)
}

type rawProtocol struct{}

func (p rawProtocol) Encode(data []byte) ([]byte, error) {
	return data, nil
}

func (p rawProtocol) Decode(data []byte) ([]byte, error) {
	return data, nil
}

var RawProtocol Protocol[[]byte] = rawProtocol{}

type Client[T any] interface {
	Ctx() context.Context
	P.Pipe[T]
	Close() error
}

type RawClient Client[[]byte]

type WSClient[T any] struct {
	util.Lifetime
	Conn     *websocket.Conn
	protocol Protocol[T]
}

const (
	WRITE_TIMEOUT = 1 * time.Second
)

func (c *WSClient[T]) Send(data T) error {
	ctx, cancel := context.WithTimeout(c.Ctx(), WRITE_TIMEOUT)
	defer cancel()

	encoded, err := c.protocol.Encode(data)
	if err != nil {
		return err
	}

	return c.Conn.Write(ctx, websocket.MessageBinary, encoded)
}

func (c *WSClient[T]) Receive() <-chan P.Packet[T] {
	ctx := c.Ctx()
	out := make(chan P.Packet[T])
	go func() {
		for {
			if ctx.Err() != nil {
				return
			}

			typ, message, err := c.Conn.Read(ctx)
			if err != nil {
				out <- P.Packet[T]{
					Error: err,
				}
				// TODO(cfoust): 05/27/23 error handling?
				c.Cancel()
				return
			}

			if typ != websocket.MessageBinary {
				continue
			}

			decoded, err := c.protocol.Decode(message)

			out <- P.Packet[T]{
				Contents: decoded,
				Error:    err,
			}
		}
	}()

	return out
}

func (c *WSClient[T]) Close() error {
	c.Cancel()
	return c.Conn.Close(websocket.StatusNormalClosure, "")
}

var _ RawClient = (*WSClient[[]byte])(nil)

func Connect[T any](ctx context.Context, protocol Protocol[T], socketPath string) (Client[T], error) {
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

	// TODO(cfoust): 06/09/23 be smarter about this
	c.SetReadLimit(32768 * 256)

	client := WSClient[T]{
		protocol: protocol,
		Lifetime: util.NewLifetime(ctx),
		Conn:     c,
	}

	go func() {
		<-client.Ctx().Done()
		_ = c.Close(websocket.StatusNormalClosure, "")
	}()

	return &client, nil
}
