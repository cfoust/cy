package cy

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/io/pipe"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"

	"github.com/stretchr/testify/assert"
)

type TestServer struct {
	baseCtx    context.Context
	cy         *Cy
	socketPath string
}

func (t *TestServer) Connect() (ws.Client, error) {
	return ws.Connect(t.baseCtx, t.socketPath)
}

func setupServer(ctx context.Context) (*TestServer, error) {
	dir, err := os.MkdirTemp("", "example")
	if err != nil {
		return nil, err
	}

	socketPath := filepath.Join(dir, "socket")

	cy := Cy{}

	go func() {
		ws.Serve(ctx, socketPath, &cy)
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

	io := pipe.Map[[]byte](
		client,
		P.Encode,
		P.Decode,
	)

	io.Send(P.HandshakeMessage{
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

	io := pipe.Map[[]byte](
		client,
		P.Encode,
		P.Decode,
	)

	err = io.Send(P.InputMessage{
		Data: []byte("hello"),
	})
	assert.NoError(t, err)

	go func() {
		for {
			<-io.Receive()
		}
	}()

	<-client.Ctx().Done()
	assert.Error(t, client.Ctx().Err())
}
