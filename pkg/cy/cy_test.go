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
	"github.com/cfoust/cy/pkg/wm"

	"github.com/stretchr/testify/assert"
)

type TestServer struct {
	util.Lifetime
	cy         *Cy
	socketPath string
}

func (t *TestServer) Connect() (Connection, *Client, error) {
	conn, err := ws.Connect(t.Ctx(), P.Protocol, t.socketPath)
	if err != nil {
		return nil, nil, err
	}

	// TODO(cfoust): 06/08/23 lol
	time.Sleep(100 * time.Millisecond)

	t.cy.Lock()
	client := t.cy.clients[len(t.cy.clients)-1]
	t.cy.Unlock()

	return conn, client, nil
}

func (t *TestServer) Attach(rows, cols int) (Connection, *Client, error) {
	conn, client, err := t.Connect()
	if err != nil {
		return nil, nil, err
	}

	conn.Send(P.HandshakeMessage{
		TERM:    "xterm-256color",
		Rows:    rows,
		Columns: cols,
	})

	return conn, client, nil
}

func (t *TestServer) Standard() (Connection, *Client, error) {
	return t.Attach(26, 80)
}

func (t *TestServer) Release() {
	t.Cancel()
}

func setupServer(t *testing.T) *TestServer {
	dir, err := os.MkdirTemp("", "example")
	assert.NoError(t, err)

	socketPath := filepath.Join(dir, "socket")

	cy, err := Start(context.Background())
	assert.NoError(t, err)

	server := TestServer{
		Lifetime:   util.NewLifetime(context.Background()),
		cy:         cy,
		socketPath: socketPath,
	}

	go func() {
		ws.Serve[P.Message](server.Ctx(), socketPath, P.Protocol, cy)
		os.RemoveAll(dir)
	}()

	// TODO(cfoust): 06/01/23 no more race condition on socket creation
	time.Sleep(100 * time.Millisecond)

	return &server
}

func TestHandshake(t *testing.T) {
	server := setupServer(t)
	defer server.Release()

	conn, _, err := server.Connect()
	assert.NoError(t, err)

	conn.Send(P.HandshakeMessage{
		TERM:    "xterm-256color",
		Rows:    26,
		Columns: 80,
	})

	time.Sleep(100 * time.Millisecond)

	assert.NoError(t, conn.Ctx().Err())
}

func TestBadHandshake(t *testing.T) {
	server := setupServer(t)
	defer server.Release()

	conn, _, err := server.Connect()
	assert.NoError(t, err)

	err = conn.Send(P.InputMessage{
		Data: []byte("hello"),
	})
	assert.NoError(t, err)

	go func() {
		for {
			<-conn.Receive()
		}
	}()

	<-conn.Ctx().Done()
	assert.Error(t, conn.Ctx().Err())
}

func TestEmpty(t *testing.T) {
	server := setupServer(t)
	defer server.Release()

	cy := server.cy
	assert.Equal(t, cy.tree, (*wm.Node)(nil))

	_, _, err := server.Standard()
	assert.NoError(t, err)
}
