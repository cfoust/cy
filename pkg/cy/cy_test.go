package cy

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"

	"github.com/stretchr/testify/assert"
)

type TestServer struct {
	baseCtx    context.Context
	cy         *Cy
	socketPath string
}

func (t *TestServer) Connect() (Connection, error) {
	client, err := ws.Connect(t.baseCtx, P.Protocol, t.socketPath)
	if err != nil {
		return nil, err
	}

	return client, nil
}

func setupServer(ctx context.Context) (*TestServer, error) {
	dir, err := os.MkdirTemp("", "example")
	if err != nil {
		return nil, err
	}

	socketPath := filepath.Join(dir, "socket")

	cy := Cy{}

	go func() {
		ws.Serve[P.Message](ctx, socketPath, P.Protocol, &cy)
		os.RemoveAll(dir)
	}()

	server := TestServer{
		baseCtx:    ctx,
		cy:         &cy,
		socketPath: socketPath,
	}

	// TODO(cfoust): 06/01/23 no more race condition on socket creation
	time.Sleep(100 * time.Millisecond)

	return &server, nil
}

func TestHandshake(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	server, err := setupServer(ctx)
	assert.NoError(t, err)

	client, err := server.Connect()
	assert.NoError(t, err)

	client.Send(P.HandshakeMessage{
		TERM:    "xterm-256color",
		Rows:    26,
		Columns: 80,
	})

	time.Sleep(100 * time.Millisecond)

	assert.NoError(t, client.Ctx().Err())
}

func TestBadHandshake(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	server, err := setupServer(ctx)
	assert.NoError(t, err)

	client, err := server.Connect()
	assert.NoError(t, err)

	err = client.Send(P.InputMessage{
		Data: []byte("hello"),
	})
	assert.NoError(t, err)

	go func() {
		for {
			<-client.Receive()
		}
	}()

	<-client.Ctx().Done()
	assert.Error(t, client.Ctx().Err())
}
