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
			name:     "enable protocol",
			sequence: "\x1b[>6;1u",
			expected: KeyDisambiguateEscape | KeyReportEventTypes,
		},
		{
			name:     "push protocol",
			sequence: "\x1b[>14;2u",
			expected: KeyDisambiguateEscape | KeyReportEventTypes | KeyReportAlternateKeys,
		},
		{
			name:     "disable protocol",
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
	term.Write([]byte("\x1b[>6;1u"))

	if term.KeyState() != mainFlags {
		t.Errorf("Expected main screen flags=%d, got %d", mainFlags, term.KeyState())
	}

	// Switch to alt screen
	term.Write([]byte("\x1b[?1049h"))

	// Set different protocol on alt screen
	altFlags := KeyReportAlternateKeys
	term.Write([]byte("\x1b[>8;1u"))

	if term.KeyState() != altFlags {
		t.Errorf("Expected alt screen flags=%d, got %d", altFlags, term.KeyState())
	}

	// Switch back to main screen - should restore original key state
	term.Write([]byte("\x1b[?1049l"))

	if term.KeyState() != mainFlags {
		t.Errorf("Expected restored main screen flags=%d, got %d", mainFlags, term.KeyState())
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
			setup:    "\x1b[>6;1u",
			expected: "\x1b[?6;1u",
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

			// Setup key state
			if tt.setup != "" {
				term.Write([]byte(tt.setup))
			}

			// Clear any previous output
			output.Reset()

			// Send query sequence
			term.Write([]byte("\x1b[?u"))

			// Check the response
			response := output.String()
			if response != tt.expected {
				t.Errorf("Expected query response '%s', got '%s'", tt.expected, response)
			}
		})
	}
}
