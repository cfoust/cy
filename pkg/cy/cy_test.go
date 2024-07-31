package cy

import (
	"context"
	"testing"

	"github.com/cfoust/cy/pkg/cy/cmd"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux/stream"

	"github.com/stretchr/testify/require"
)

func setup(t *testing.T) (*Cy, func(geom.Size) *Client) {
	ctx := context.Background()
	cy, err := Start(ctx, Options{
		Shell: "/bin/bash",
	})
	require.NoError(t, err)

	return cy, func(size geom.Size) *Client {
		client, err := cy.NewClient(ctx, ClientOptions{
			Env: map[string]string{
				"TERM": "xterm-256color",
			},
			Size: geom.DEFAULT_SIZE,
		})
		require.NoError(t, err)
		return client
	}
}

func TestEmpty(t *testing.T) {
	server, create := setup(t)
	client := create(geom.DEFAULT_SIZE)
	leaves := server.tree.Leaves()
	require.Equal(t, client.Node(), leaves[1])
}

func TestScopes(t *testing.T) {
	server, create := setup(t)

	client := create(geom.DEFAULT_SIZE)
	require.Equal(t, 3, len(client.binds.Scopes()))

	group := server.tree.Root().NewGroup()
	cmd, err := cmd.New(
		server.Ctx(),
		stream.CmdOptions{
			Command: "/bin/bash",
		},
		"",
		server.timeBinds,
		server.copyBinds,
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

func TestShell(t *testing.T) {
	_, create := setup(t)
	client := create(geom.DEFAULT_SIZE)
	require.NoError(t, client.execute(`
(shell/new)
`))
}

func TestClients(t *testing.T) {
	_, create := setup(t)

	numClients := 20
	var clients []*Client
	for i := 0; i < numClients; i++ {
		clients = append(clients, create(geom.DEFAULT_SIZE))
	}

	for i := 0; i < numClients; i++ {
		clients[i].Cancel()
	}
}
