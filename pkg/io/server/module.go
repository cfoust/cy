package server

import (
	"context"
	"net"
	"net/http"
	"time"

	"github.com/cfoust/cy/pkg/util"

	"nhooyr.io/websocket"
)

type Client interface {
	Ctx() context.Context
	Send(data []byte) error
	Receive() <-chan []byte
	Disconnect(err error)
}

type Server interface {
	HandleClient(client Client)
}

type WSClient struct {
	util.Session
	conn *websocket.Conn
}

const (
	WRITE_TIMEOUT = 1 * time.Second
)

func (c *WSClient) Send(data []byte) error {
	ctx, cancel := context.WithTimeout(c.Ctx(), WRITE_TIMEOUT)
	defer cancel()
	return c.conn.Write(ctx, websocket.MessageBinary, data)
}

func (c *WSClient) Receive() <-chan []byte {
	ctx := c.Ctx()
	out := make(chan []byte)
	go func() {
		for {
			if ctx.Err() != nil {
				return
			}

			typ, message, err := c.conn.Read(ctx)
			if err != nil {
				return
			}

			if typ != websocket.MessageBinary {
				continue
			}

			out <- message
		}
	}()

	return out
}

func (c *WSClient) Disconnect(err error) {
	c.Cancel()
}

var _ Client = (*WSClient)(nil)

type WSServer struct {
	server Server
}

func (ws *WSServer) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	c, err := websocket.Accept(w, r, &websocket.AcceptOptions{
		OriginPatterns:  []string{"*"},
		CompressionMode: websocket.CompressionDisabled,
	})

	if err != nil {
		return
	}

	defer c.Close(websocket.StatusInternalError, "operational fault during relay")

	client := WSClient{
		Session: util.NewSession(r.Context()),
		conn:    c,
	}

	done := make(chan bool)
	go func() {
		ws.server.HandleClient(&client)
		done <- true
	}()

	select {
	case <-client.Ctx().Done():
		return
	case <-done:
		client.Cancel()
		return
	}
}

func Serve(socketPath string, server Server) error {
	l, err := net.Listen("unix", socketPath)
	if err != nil {
		return err
	}

	ws := &WSServer{server: server}
	httpServer := http.Server{
		Handler: ws,
	}

	return httpServer.Serve(l)
}
