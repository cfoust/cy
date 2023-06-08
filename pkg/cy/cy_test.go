package cy

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"
	"github.com/cfoust/cy/pkg/util"

	"github.com/stretchr/testify/assert"
)

type TestServer struct {
	util.Lifetime
	cy         *Cy
	socketPath string
}

func (t *TestServer) Connect() (Connection, error) {
	client, err := ws.Connect(t.Ctx(), P.Protocol, t.socketPath)
	if err != nil {
		return nil, err
	}

	return client, nil
}

func (t *TestServer) Release() {
	t.Cancel()
}

func setupServer(t *testing.T) *TestServer {
	dir, err := os.MkdirTemp("", "example")
	assert.NoError(t, err)

	socketPath := filepath.Join(dir, "socket")

	cy := Cy{}

	server := TestServer{
		Lifetime:   util.NewLifetime(context.Background()),
		cy:         &cy,
		socketPath: socketPath,
	}

	go func() {
		ws.Serve[P.Message](server.Ctx(), socketPath, P.Protocol, &cy)
		os.RemoveAll(dir)
	}()

	// TODO(cfoust): 06/01/23 no more race condition on socket creation
	time.Sleep(100 * time.Millisecond)

	return &server
}

func TestHandshake(t *testing.T) {
	server := setupServer(t)
	defer server.Release()

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
	server := setupServer(t)
	defer server.Release()

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
