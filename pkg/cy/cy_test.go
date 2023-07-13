package cy

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/app"
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"
	"github.com/cfoust/cy/pkg/util"

	"github.com/stretchr/testify/require"
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

	time.Sleep(100 * time.Millisecond)

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
	require.NoError(t, err)

	socketPath := filepath.Join(dir, "socket")

	cy, err := Start(context.Background(), "")
	require.NoError(t, err)

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
	require.NoError(t, err)

	conn.Send(P.HandshakeMessage{
		TERM:    "xterm-256color",
		Rows:    26,
		Columns: 80,
	})

	time.Sleep(100 * time.Millisecond)

	require.NoError(t, conn.Ctx().Err())
}

func TestBadHandshake(t *testing.T) {
	server := setupServer(t)
	defer server.Release()

	conn, _, err := server.Connect()
	require.NoError(t, err)

	err = conn.Send(P.InputMessage{
		Data: []byte("hello"),
	})
	require.NoError(t, err)

	go func() {
		for {
			<-conn.Receive()
		}
	}()

	<-conn.Ctx().Done()
	require.Error(t, conn.Ctx().Err())
}

func TestEmpty(t *testing.T) {
	server := setupServer(t)
	defer server.Release()

	cy := server.cy

	_, client, err := server.Standard()
	require.NoError(t, err)

	time.Sleep(100 * time.Millisecond)

	leaves := cy.tree.Leaves()
	require.Equal(t, client.GetNode(), leaves[0])
}

func TestSize(t *testing.T) {
	server := setupServer(t)
	defer server.Release()

	_, _, err := server.Attach(40, 80)
	require.NoError(t, err)

	_, _, err = server.Standard()
	require.NoError(t, err)

	time.Sleep(100 * time.Millisecond)
}

func TestScopes(t *testing.T) {
	server := setupServer(t)
	defer server.Release()

	_, client, err := server.Standard()
	require.NoError(t, err)
	require.Equal(t, 2, len(client.binds.Scopes()))

	cy := server.cy

	group := cy.tree.Root().NewGroup()
	pane := group.NewCmd(
		server.Ctx(),
		app.CmdOptions{
			Command: "/bin/bash",
		},
		geom.DEFAULT_SIZE,
	)
	err = client.Attach(pane)
	require.NoError(t, err)
	require.Equal(t, 3, len(client.binds.Scopes()))
}
