package emu

import (
	"bytes"
	"testing"
)

func TestKittyProtocolCSI(t *testing.T) {
	var tests = []struct {
		name     string
		sequence string
		expected KeyProtocol
	}{
		{
			name:     "set",
			sequence: "\x1b[=1u",
			expected: KeyDisambiguateEscape,
		},
		{
			name:     "push 1",
			sequence: "\x1b[>31u",
			expected: KeyDisambiguateEscape | KeyReportEventTypes | KeyReportAlternateKeys | KeyReportAllKeys | KeyReportAssociatedText,
		},
		{
			name:     "pop 1",
			sequence: "\x1b[<u",
			expected: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			term := New()

			// Send the sequence to the terminal
			term.Write([]byte(tt.sequence))

			// Get the key state from the terminal
			flags := term.KeyState()
			if flags != tt.expected {
				t.Errorf("Expected flags=%d, got %d", tt.expected, flags)
			}
		})
	}
}

func TestKeyStateScreenSwapping(t *testing.T) {
	term := New()

	// Enable protocol on main screen
	mainFlags := KeyDisambiguateEscape | KeyReportEventTypes
	term.Write([]byte("\x1b[=3u"))

	if term.KeyState() != mainFlags {
		t.Errorf(
			"Expected main screen flags=%d, got %d",
			mainFlags,
			term.KeyState(),
		)
	}

	// Switch to alt screen
	term.Write([]byte("\x1b[?1049h"))

	// Set different protocol on alt screen
	altFlags := KeyReportAlternateKeys
	term.Write([]byte("\x1b[=4u"))

	if term.KeyState() != altFlags {
		t.Errorf(
			"Expected alt screen flags=%d, got %d",
			altFlags,
			term.KeyState(),
		)
	}

	// Switch back to main screen - should restore original key state
	term.Write([]byte("\x1b[?1049l"))

	if term.KeyState() != mainFlags {
		t.Errorf(
			"Expected restored main screen flags=%d, got %d",
			mainFlags,
			term.KeyState(),
		)
	}
}

func TestKeyStateQuery(t *testing.T) {
	var tests = []struct {
		name     string
		setup    string
		expected string
	}{
		{
			name:     "query disabled state",
			setup:    "",
			expected: "\x1b[?0u",
		},
		{
			name:     "query enabled state",
			setup:    "\x1b[>6u",
			expected: "\x1b[?6u",
		},
		{
			name:     "query after disable",
			setup:    "\x1b[>6;1u\x1b[<u",
			expected: "\x1b[?0u",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var output bytes.Buffer
			term := New(WithWriter(&output))

			if tt.setup != "" {
				term.Write([]byte(tt.setup))
			}

			output.Reset()

			term.Write([]byte("\x1b[?u"))

			response := output.String()
			if response != tt.expected {
				t.Errorf(
					"Expected query response '%s', got '%s'",
					tt.expected,
					response,
				)
			}
		})
	}
}
