package main

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestParseTarget(t *testing.T) {
	require.Equal(t, "session", parseTarget("session"))
	require.Equal(t, "session", parseTarget("session:0"))
	require.Equal(t, "session", parseTarget("session:0.0"))
	require.Equal(
		t,
		"mngr-agent1",
		parseTarget("mngr-agent1:0"),
	)
}

func TestQuoteJanet(t *testing.T) {
	require.Equal(t, `"hello"`, quoteJanet("hello"))
	require.Equal(
		t,
		`"hello \"world\""`,
		quoteJanet(`hello "world"`),
	)
	require.Equal(
		t,
		`"line1\nline2"`,
		quoteJanet("line1\nline2"),
	)
}

func TestParseTmuxFormat(t *testing.T) {
	parts := parseTmuxFormat(
		"#{pane_dead}|#{pane_current_command}|#{pane_pid}",
	)
	require.Equal(t, 5, len(parts))
	require.Equal(
		t,
		formatPart{isVariable: true, text: "pane_dead"},
		parts[0],
	)
	require.Equal(
		t,
		formatPart{isVariable: false, text: "|"},
		parts[1],
	)
	require.Equal(
		t,
		formatPart{
			isVariable: true,
			text:       "pane_current_command",
		},
		parts[2],
	)
	require.Equal(
		t,
		formatPart{isVariable: false, text: "|"},
		parts[3],
	)
	require.Equal(
		t,
		formatPart{isVariable: true, text: "pane_pid"},
		parts[4],
	)

	// Single char format
	parts = parseTmuxFormat("#I")
	require.Equal(t, 1, len(parts))
	require.Equal(
		t,
		formatPart{isVariable: true, text: "I"},
		parts[0],
	)

	// Just a variable
	parts = parseTmuxFormat("#{pane_pid}")
	require.Equal(t, 1, len(parts))
	require.Equal(
		t,
		formatPart{isVariable: true, text: "pane_pid"},
		parts[0],
	)
}

func TestTmuxKeyToCy(t *testing.T) {
	require.Equal(t, "enter", tmuxKeyToCy("Enter"))
	require.Equal(t, "space", tmuxKeyToCy("Space"))
	require.Equal(t, "ctrl+c", tmuxKeyToCy("C-c"))
	require.Equal(t, "alt+x", tmuxKeyToCy("M-x"))
	require.Equal(t, "f1", tmuxKeyToCy("F1"))
}

func TestFirstPaneExpr(t *testing.T) {
	expr := firstPaneExpr("mngr-agent1")
	require.Contains(t, expr, "/mngr-agent1")
	require.Contains(t, expr, "group/leaves")
	require.Contains(t, expr, "tree/id")

	// With window suffix
	expr = firstPaneExpr("mngr-agent1:0")
	require.Contains(t, expr, "/mngr-agent1")
	require.NotContains(t, expr, ":0")
}
