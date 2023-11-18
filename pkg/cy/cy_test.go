package cy

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/cy/cmd"
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"
	"github.com/cfoust/cy/pkg/mux/stream"
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
		Env: map[string]string{
			"TERM": "xterm-256color",
		},
		Size: geom.Size{
			R: rows,
			C: cols,
		},
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

	cy, err := Start(context.Background(), Options{
		DataDir: filepath.Join(t.TempDir(), "data"),
	})
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
		Env: map[string]string{
			"TERM": "xterm-256color",
		},
		Size: geom.Size{
			R: 26,
			C: 80,
		},
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
	require.Equal(t, client.Node(), leaves[0])
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
	cmd, err := cmd.New(
		server.Ctx(),
		stream.CmdOptions{
			Command: "/bin/bash",
		},
		"",
		cy.replayBinds,
	)
	require.NoError(t, err)
	pane := group.NewPane(
		server.Ctx(),
		cmd,
	)
	require.NoError(t, err)
	err = client.Attach(pane)
	require.NoError(t, err)
	require.Equal(t, 3, len(client.binds.Scopes()))
}

func TestPaneKill(t *testing.T) {
	server := setupServer(t)
	defer server.Release()

	_, client, err := server.Standard()
	require.NoError(t, err)

	require.NoError(t, client.execute(`
(def shell (cmd/new (tree/root) "/tmp"))
(pp shell)
(pane/attach shell)
(tree/kill shell)
`))
	time.Sleep(2 * time.Second) // lol
	leaves := server.cy.tree.Leaves()
	require.Equal(t, leaves[0].Id(), client.Node().Id())
}
