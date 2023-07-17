package stream

import (
	"context"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/geom"

	"github.com/stretchr/testify/require"
)

func TestHealthy(t *testing.T) {
	cmd, err := NewCmd(
		context.Background(),
		CmdOptions{
			Command: "/bin/sh",
		},
		geom.Vec2{
			Rows:    26,
			Columns: 80,
		},
	)
	require.NoError(t, err)

	time.Sleep(100 * time.Millisecond)

	require.Equal(t, cmd.GetStatus(), CmdStatusHealthy)
}

func TestFailLoop(t *testing.T) {
	cmd, _ := NewCmd(
		context.Background(),
		CmdOptions{
			Command: "/bin/sh",
			Args: []string{
				"-c",
				"exit 1",
			},
		},
		geom.Vec2{
			Rows:    26,
			Columns: 80,
		},
	)

	time.Sleep(1 * time.Second)

	require.Equal(t, cmd.GetStatus(), CmdStatusFailed)
}
