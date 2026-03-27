package stream

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/geom"

	"github.com/stretchr/testify/require"
)

func waitForStatus(
	ctx context.Context,
	cmd *Cmd,
	want CmdStatus,
	timeout time.Duration,
) error {
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()

	changes := cmd.SubscribeStatus(ctx)

	if cmd.Status() == want {
		return nil
	}

	for {
		select {
		case <-ctx.Done():
			return fmt.Errorf(
				"timed out waiting for status %d, current: %d",
				want,
				cmd.Status(),
			)
		case status := <-changes.Recv():
			if status == want {
				return nil
			}
		}
	}
}

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

	require.Equal(t, CmdStatusHealthy, cmd.Status())
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

	require.Equal(t, CmdStatusFailed, cmd.Status())
}

func TestComplete(t *testing.T) {
	ctx := context.Background()
	cmd, _ := NewCmd(
		ctx,
		CmdOptions{
			Command: "/bin/sh",
			Args: []string{
				"-c",
				"exit 0",
			},
		},
		geom.DEFAULT_SIZE,
	)

	err := waitForStatus(ctx, cmd, CmdStatusComplete, 5*time.Second)
	require.NoError(t, err)
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

	cancel()

	err := waitForStatus(
		context.Background(),
		cmd,
		CmdStatusKilled,
		5*time.Second,
	)
	require.NoError(t, err)
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

	require.Equal(t, CmdStatusFailed, cmd.Status())
	err := <-errc
	require.Error(t, err)
	require.Error(t, cmd.ExitError())
}
