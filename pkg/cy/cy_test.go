package cy

import (
	"context"
	"testing"
	"time"

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

func TestPaneKill(t *testing.T) {
	server, create := setup(t)
	client := create(geom.DEFAULT_SIZE)

	leaves := server.tree.Leaves()
	id := client.Node().Id()
	require.Equal(t, leaves[1].Id(), id)

	require.NoError(t, client.execute(`
(tree/kill (pane/current))
`))
	time.Sleep(2 * time.Second) // lol
	require.NotEqual(t, id, client.Node().Id())
	leaves = server.tree.Leaves()
	require.Equal(t, leaves[1].Id(), client.Node().Id())
}

func TestShell(t *testing.T) {
	_, create := setup(t)
	client := create(geom.DEFAULT_SIZE)
	require.NoError(t, client.execute(`
(shell/new)
`))
}
