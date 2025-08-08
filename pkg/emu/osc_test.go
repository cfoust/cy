package emu

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestTitle(t *testing.T) {
	term := New()
	_, _ = term.Write([]byte("\x1b]0;test\x1b\\"))
	require.Equal(t, "test", term.Title())
}

func TestPWD(t *testing.T) {
	term := New(WithoutHistory)
	_, _ = term.Write([]byte("\x1b]7;test\x1b\\"))
	require.Equal(t, "test", term.Directory())
}
