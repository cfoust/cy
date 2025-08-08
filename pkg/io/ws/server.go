package ws

import (
	"context"
	"net"
	"net/http"
	"os"

	"github.com/cfoust/cy/pkg/util"

	"nhooyr.io/websocket"
)

type Server[T any] interface {
	HandleWSClient(client Client[T])
}

type WSServer[T any] struct {
	server   Server[T]
	protocol Protocol[T]
}

func (ws *WSServer[T]) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	c, err := websocket.Accept(w, r, &websocket.AcceptOptions{
		OriginPatterns:  []string{"*"},
		CompressionMode: websocket.CompressionDisabled,
	})

	if err != nil {
		return
	}

	defer func() { _ = c.Close(websocket.StatusInternalError, "operational fault during relay") }()

	client := WSClient[T]{
		Lifetime: util.NewLifetime(r.Context()),
		Conn:     c,
		protocol: ws.protocol,
	}

	done := make(chan bool)
	go func() {
		ws.server.HandleWSClient(&client)
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

var _ http.Handler = (*WSServer[[]byte])(nil)

func Serve[T any](
	ctx context.Context,
	socketPath string,
	protocol Protocol[T],
	server Server[T],
) error {
	l, err := net.Listen("unix", socketPath)
	if err != nil {
		return err
	}

	ws := &WSServer[T]{server: server, protocol: protocol}
	httpServer := http.Server{
		Handler: ws,
	}

	go func() {
		<-ctx.Done()
		_ = httpServer.Shutdown(ctx)
		_ = l.Close()
		_ = os.Remove(socketPath)
	}()

	return httpServer.Serve(l)
}
