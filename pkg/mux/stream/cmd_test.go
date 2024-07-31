package stream

import (
	"bytes"
	"context"
	"io"
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
		geom.DEFAULT_SIZE,
	)
	require.NoError(t, err)

	time.Sleep(100 * time.Millisecond)

	require.Equal(t, CmdStatusHealthy, cmd.status)
}

func TestFailLoop(t *testing.T) {
	cmd, _ := NewCmd(
		context.Background(),
		CmdOptions{
			Command: "/bin/sh",
			Restart: true,
			Args: []string{
				"-c",
				"exit 1",
			},
		},
		geom.DEFAULT_SIZE,
	)

	time.Sleep(1 * time.Second)

	require.Equal(t, CmdStatusFailed, cmd.status)
}

func TestComplete(t *testing.T) {
	cmd, _ := NewCmd(
		context.Background(),
		CmdOptions{
			Command: "/bin/sh",
			Args: []string{
				"-c",
				"sleep 1 && exit 0",
			},
		},
		geom.DEFAULT_SIZE,
	)

	time.Sleep(1*time.Second + 250*time.Millisecond)

	require.Equal(t, CmdStatusComplete, cmd.status)
}

func TestKilled(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	cmd, _ := NewCmd(
		ctx,
		CmdOptions{
			Command: "/bin/sh",
			Args: []string{
				"-c",
				"sleep 5",
			},
		},
		geom.DEFAULT_SIZE,
	)

	time.Sleep(1 * time.Second)
	cancel()
	time.Sleep(250 * time.Millisecond)

	require.Equal(t, CmdStatusKilled, cmd.status)
}

func TestFailed(t *testing.T) {
	cmd, _ := NewCmd(
		context.Background(),
		CmdOptions{
			Command: "/bin/sh",
			Args: []string{
				"-c",
				"exit 255",
			},
		},
		geom.DEFAULT_SIZE,
	)

	errc := make(chan error)
	go func() {
		var b bytes.Buffer
		_, err := io.Copy(&b, cmd)
		errc <- err
	}()

	time.Sleep(250 * time.Millisecond)

	require.Equal(t, CmdStatusFailed, cmd.status)
	err := <-errc
	require.Error(t, err)
	require.Error(t, cmd.exitError)
}
