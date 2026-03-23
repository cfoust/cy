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

func TestOSC133PromptStart(t *testing.T) {
	term := New()
	_, _ = term.Write([]byte("\x1b]133;A\x1b\\"))

	dirty := term.Changes()
	require.True(t, dirty.HasSemanticPrompt())

	prompts := dirty.GetSemanticPrompts()
	require.Len(t, prompts, 1)
	require.Equal(t, PromptStart, prompts[0].Type)
	require.Nil(t, prompts[0].ExitCode)
}

func TestOSC133CommandStart(t *testing.T) {
	term := New()
	_, _ = term.Write([]byte("\x1b]133;B\x1b\\"))

	dirty := term.Changes()
	prompts := dirty.GetSemanticPrompts()
	require.Len(t, prompts, 1)
	require.Equal(t, CommandStart, prompts[0].Type)
}

func TestOSC133CommandExecuted(t *testing.T) {
	term := New()
	_, _ = term.Write([]byte("\x1b]133;C\x1b\\"))

	dirty := term.Changes()
	prompts := dirty.GetSemanticPrompts()
	require.Len(t, prompts, 1)
	require.Equal(t, CommandExecuted, prompts[0].Type)
}

func TestOSC133CommandFinished(t *testing.T) {
	term := New()
	_, _ = term.Write([]byte("\x1b]133;D;0\x1b\\"))

	dirty := term.Changes()
	prompts := dirty.GetSemanticPrompts()
	require.Len(t, prompts, 1)
	require.Equal(t, CommandFinished, prompts[0].Type)
	require.NotNil(t, prompts[0].ExitCode)
	require.Equal(t, 0, *prompts[0].ExitCode)
}

func TestOSC133CommandFinishedNonZero(t *testing.T) {
	term := New()
	_, _ = term.Write([]byte("\x1b]133;D;1\x1b\\"))

	dirty := term.Changes()
	prompts := dirty.GetSemanticPrompts()
	require.Len(t, prompts, 1)
	require.Equal(t, CommandFinished, prompts[0].Type)
	require.NotNil(t, prompts[0].ExitCode)
	require.Equal(t, 1, *prompts[0].ExitCode)
}

func TestOSC133CommandFinishedWithoutExitCode(t *testing.T) {
	term := New()
	_, _ = term.Write([]byte("\x1b]133;D\x1b\\"))

	dirty := term.Changes()
	prompts := dirty.GetSemanticPrompts()
	require.Len(t, prompts, 1)
	require.Equal(t, CommandFinished, prompts[0].Type)
	require.Nil(t, prompts[0].ExitCode)
}

func TestOSC133BellTerminated(t *testing.T) {
	term := New()
	_, _ = term.Write([]byte("\x1b]133;A\x07"))

	dirty := term.Changes()
	prompts := dirty.GetSemanticPrompts()
	require.Len(t, prompts, 1)
	require.Equal(t, PromptStart, prompts[0].Type)
}

func TestOSC133MultipleEvents(t *testing.T) {
	term := New()
	// Simulate a full command cycle
	_, _ = term.Write([]byte("\x1b]133;A\x1b\\"))   // Prompt start
	_, _ = term.Write([]byte("$ "))                 // Prompt text
	_, _ = term.Write([]byte("\x1b]133;B\x1b\\"))   // Command start
	_, _ = term.Write([]byte("ls"))                 // User types command
	_, _ = term.Write([]byte("\x1b]133;C\x1b\\"))   // Command executed
	_, _ = term.Write([]byte("file1 file2\n"))      // Output
	_, _ = term.Write([]byte("\x1b]133;D;0\x1b\\")) // Command finished

	dirty := term.Changes()
	prompts := dirty.GetSemanticPrompts()
	require.Len(t, prompts, 4)

	require.Equal(t, PromptStart, prompts[0].Type)
	require.Equal(t, CommandStart, prompts[1].Type)
	require.Equal(t, CommandExecuted, prompts[2].Type)
	require.Equal(t, CommandFinished, prompts[3].Type)
	require.Equal(t, 0, *prompts[3].ExitCode)
}

func TestOSC133WriteID(t *testing.T) {
	term := New()
	_, _ = term.Write([]byte("\x1b]133;A\x1b\\"))

	dirty := term.Changes()
	prompts := dirty.GetSemanticPrompts()
	require.Len(t, prompts, 1)
	// WriteID should match the last write
	require.Equal(t, dirty.LastWrite(), prompts[0].WriteID)
}

func TestOSC8Hyperlink(t *testing.T) {
	term := New()
	// OSC 8 ; ; URI ST  (no params)
	_, _ = term.Write(
		[]byte(
			"\033]8;;https://example.com\033\\link\033]8;;\033\\",
		),
	)

	// "link" glyphs should carry the URI
	for i := 0; i < 4; i++ {
		attr := term.Cell(i, 0)
		require.Equal(
			t,
			"https://example.com",
			attr.Hyperlink.URI,
			"cell %d should have hyperlink URI", i,
		)
	}

	// Character after the close should have no hyperlink
	attr := term.Cell(4, 0)
	require.Equal(t, "", attr.Hyperlink.URI)
}

func TestOSC8HyperlinkWithID(t *testing.T) {
	term := New()
	_, _ = term.Write(
		[]byte(
			"\033]8;id=foo;https://example.com\033\\link\033]8;;\033\\",
		),
	)

	attr := term.Cell(0, 0)
	require.Equal(
		t, "https://example.com", attr.Hyperlink.URI,
	)
	require.Equal(t, "foo", attr.Hyperlink.ID)
}

func TestOSC8HyperlinkReset(t *testing.T) {
	term := New()
	// Set a hyperlink, then SGR 0 should also clear it
	_, _ = term.Write(
		[]byte(
			"\033]8;;https://example.com\033\\\033[mafter",
		),
	)

	attr := term.Cell(0, 0)
	require.Equal(t, "", attr.Hyperlink.URI,
		"SGR 0 should clear hyperlink")
}

func TestOSC133Reset(t *testing.T) {
	term := New()
	_, _ = term.Write([]byte("\x1b]133;A\x1b\\"))

	dirty := term.Changes()
	require.True(t, dirty.HasSemanticPrompt())

	dirty.Reset()
	require.False(t, dirty.HasSemanticPrompt())
	require.Empty(t, dirty.GetSemanticPrompts())
}
