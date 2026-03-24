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
	tests := []struct {
		name  string
		input string
		cells []struct {
			col  int
			uri  string
			id   string
			desc string
		}
	}{
		{
			name: "basic hyperlink",
			input: "\033]8;;https://example.com" +
				"\033\\link\033]8;;\033\\",
			cells: []struct {
				col  int
				uri  string
				id   string
				desc string
			}{
				{0, "https://example.com", "", "cell 0"},
				{1, "https://example.com", "", "cell 1"},
				{2, "https://example.com", "", "cell 2"},
				{3, "https://example.com", "", "cell 3"},
				{4, "", "", "after close"},
			},
		},
		{
			name: "hyperlink with id",
			input: "\033]8;id=foo;https://example.com" +
				"\033\\link\033]8;;\033\\",
			cells: []struct {
				col  int
				uri  string
				id   string
				desc string
			}{
				{
					0,
					"https://example.com",
					"foo",
					"should have URI and ID",
				},
			},
		},
		{
			name: "SGR 0 clears hyperlink",
			input: "\033]8;;https://example.com" +
				"\033\\\033[mafter",
			cells: []struct {
				col  int
				uri  string
				id   string
				desc string
			}{
				{
					0, "", "",
					"SGR 0 should clear hyperlink",
				},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			term := New()
			_, _ = term.Write([]byte(tt.input))

			for _, c := range tt.cells {
				g := term.Cell(c.col, 0)
				var (
					uri string
					id  string
				)
				if g.Hyperlink != nil {
					uri = g.Hyperlink.URI
					id = g.Hyperlink.ID
				}
				require.Equal(
					t, c.uri, uri, c.desc,
				)
				if c.id != "" {
					require.Equal(
						t, c.id, id,
						c.desc,
					)
				}
			}
		})
	}
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
