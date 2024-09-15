package emu

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestTitle(t *testing.T) {
	term := New()
	term.Write([]byte("\x1b]0;test\x1b\\"))
	require.Equal(t, "test", term.Title())
}

func TestPWD(t *testing.T) {
	term := New(WithoutHistory)
	term.Write([]byte("\x1b]7;test\x1b\\"))
	require.Equal(t, "test", term.Directory())
}
