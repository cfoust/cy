package main

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/cy"
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/io/ws"
	"github.com/cfoust/cy/pkg/util"

	"github.com/stretchr/testify/require"
)

type TestServer struct {
	util.Lifetime
	cy         *cy.Cy
	socketPath string
}

func (t *TestServer) Connect() (Connection, error) {
	conn, err := ws.Connect(t.Ctx(), P.Protocol, t.socketPath)
	if err != nil {
		return nil, err
	}

	// TODO(cfoust): 06/08/23 lol
	time.Sleep(100 * time.Millisecond)

	return conn, nil
}

func (t *TestServer) Attach(rows, cols int) (Connection, error) {
	conn, err := t.Connect()
	if err != nil {
		return nil, err
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

	return conn, nil
}

func (t *TestServer) Standard() (Connection, error) {
	return t.Attach(26, 80)
}

func (t *TestServer) Release() {
	t.Cancel()
}

func setupServer(t *testing.T) *TestServer {
	dir, err := os.MkdirTemp("", "example")
	require.NoError(t, err)

	socketPath := filepath.Join(dir, "socket")

	cy, err := cy.Start(context.Background(), cy.Options{
		DataDir: filepath.Join(t.TempDir(), "data"),
		Shell:   "/bin/bash",
	})
	require.NoError(t, err)

	server := &Server{cy: cy}

	testServer := TestServer{
		Lifetime:   util.NewLifetime(context.Background()),
		cy:         cy,
		socketPath: socketPath,
	}

	go func() {
		ws.Serve[P.Message](
			testServer.Ctx(),
			socketPath,
			P.Protocol,
			server,
		)
		os.RemoveAll(dir)
	}()

	// TODO(cfoust): 06/01/23 no more race condition on socket creation
	time.Sleep(100 * time.Millisecond)

	return &testServer
}

func TestHandshake(t *testing.T) {
	server := setupServer(t)
	defer server.Release()

	conn, err := server.Connect()
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

	conn, err := server.Connect()
	require.NoError(t, err)

	err = conn.Send(P.InputMessage{
		Data: []byte("hello"),
	})
	require.NoError(t, err)

	events := conn.Subscribe(conn.Ctx())

	go func() {
		for {
			<-events.Recv()
		}
	}()

	<-conn.Ctx().Done()
	require.Error(t, conn.Ctx().Err())
}

func TestExec(t *testing.T) {
	server := setupServer(t)
	defer server.Release()

	for _, test := range []struct {
		Args   RPCExecArgs
		Result []byte
	}{
		{
			Args: RPCExecArgs{
				Code: []byte(`(pp "hello")`),
			},
			Result: nil,
		},
		{
			Args: RPCExecArgs{
				Code:   []byte(`(yield 2)`),
				Format: OutputFormatRaw,
			},
			Result: []byte(`2`),
		},
		{
			Args: RPCExecArgs{
				Code:   []byte(`(yield {:a 2})`),
				Format: OutputFormatJSON,
			},
			Result: []byte(`{"a":2}`),
		},
		{
			Args: RPCExecArgs{
				Code:   []byte(`(yield {:a 2})`),
				Format: OutputFormatJanet,
			},
			Result: []byte(`{:a 2}`),
		},
	} {
		conn, err := server.Connect()
		require.NoError(t, err)

		result, err := RPC[RPCExecArgs, RPCExecResponse](
			conn,
			RPCExec,
			test.Args,
		)
		require.NoError(t, err)
		require.NotNil(t, result)
		require.Equal(t, test.Result, result.Data)

		conn.Close()
	}

}
