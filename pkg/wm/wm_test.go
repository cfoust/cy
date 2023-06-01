package wm

import (
	"context"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestHealthy(t *testing.T) {
	pane := NewPane(
		context.Background(),
		PaneContext{
			Command: "/bin/sh",
		},
		Size{
			Rows: 26,
			Cols: 80,
		},
	)

	time.Sleep(100 * time.Millisecond)

	assert.Equal(t, pane.GetStatus(), PaneStatusHealthy)
}

func TestFailLoop(t *testing.T) {
	pane := NewPane(
		context.Background(),
		PaneContext{
			Command: "/bin/sh",
			Args: []string{
				"-c",
				"exit 1",
			},
		},
		Size{
			Rows: 26,
			Cols: 80,
		},
	)

	time.Sleep(500 * time.Millisecond)

	assert.Equal(t, pane.GetStatus(), PaneStatusFailed)
}
