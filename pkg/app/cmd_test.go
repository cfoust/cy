package app

import (
	"context"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/geom"

	"github.com/stretchr/testify/assert"
)

func TestHealthy(t *testing.T) {
	cmd := NewCmd(
		context.Background(),
		CmdOptions{
			Command: "/bin/sh",
		},
		geom.Size{
			Rows:    26,
			Columns: 80,
		},
	)

	time.Sleep(100 * time.Millisecond)

	assert.Equal(t, cmd.GetStatus(), CmdStatusHealthy)
}

func TestFailLoop(t *testing.T) {
	cmd := NewCmd(
		context.Background(),
		CmdOptions{
			Command: "/bin/sh",
			Args: []string{
				"-c",
				"exit 1",
			},
		},
		geom.Size{
			Rows:    26,
			Columns: 80,
		},
	)

	time.Sleep(500 * time.Millisecond)

	assert.Equal(t, cmd.GetStatus(), CmdStatusFailed)
}
