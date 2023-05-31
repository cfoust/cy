package ws

import (
	"context"
	"net"
	"net/http"

	"github.com/cfoust/cy/pkg/util"

	"nhooyr.io/websocket"
)

type Server interface {
	HandleClient(client Client)
}

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
		Lifetime: util.NewLifetime(r.Context()),
		Conn:     c,
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

var _ http.Handler = (*WSServer)(nil)

func Serve(ctx context.Context, socketPath string, server Server) error {
	l, err := net.Listen("unix", socketPath)
	if err != nil {
		return err
	}

	ws := &WSServer{server: server}
	httpServer := http.Server{
		Handler: ws,
	}

	go func() {
		select {
		case <-ctx.Done():
			httpServer.Shutdown(ctx)
		}
	}()

	return httpServer.Serve(l)
}
