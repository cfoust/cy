package cy

import (
	"context"
	"io"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/clipboard"
	"github.com/cfoust/cy/pkg/geom"
	S "github.com/cfoust/cy/pkg/mux/screen"
	T "github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/replay"
	"github.com/cfoust/cy/pkg/replay/replayable"

	"github.com/stretchr/testify/require"
)

// TestClipboardSystemPath tests the UseSystemClipboard=true path,
// which writes to the server's MemoryClipboard.
func TestClipboardSystemPath(t *testing.T) {
	server, create, err := NewTestServer()
	require.NoError(t, err)
	defer server.Cancel()

	client, err := create(geom.DEFAULT_SIZE)
	require.NoError(t, err)
	defer client.Cancel()

	mc, ok := server.options.Clipboard.(*clipboard.MemoryClipboard)
	require.True(t, ok, "expected MemoryClipboard in test server")

	t.Log("calling clipboard/set via Janet...")
	err = client.execute(`(clipboard/set "hello-system")`)
	require.NoError(t, err)
	t.Log("clipboard/set returned without error")

	val, err := mc.Read()
	require.NoError(t, err)
	t.Log("MemoryClipboard value:", val)
	require.Equal(t, "hello-system", val)
}

// TestClipboardOSC52Path tests the UseSystemClipboard=false path,
// which writes OSC52 to the renderer pipe.
func TestClipboardOSC52Path(t *testing.T) {
	ctx := context.Background()
	cy, err := Start(ctx, Options{
		Shell:     "/bin/bash",
		SkipInput: true,
		StateDir:  "",
	})
	require.NoError(t, err)
	defer cy.Cancel()

	t.Log("UseSystemClipboard default:",
		cy.defaultParams.UseSystemClipboard())

	client, err := cy.NewClient(ctx, ClientOptions{
		Env: map[string]string{
			"TERM": "xterm-256color",
		},
		Size: geom.DEFAULT_SIZE,
	})
	require.NoError(t, err)
	defer client.Cancel()

	// Start reading from the renderer to prevent the pipe from blocking.
	output := make(chan []byte, 100)
	go func() {
		buf := make([]byte, 4096)
		for {
			n, err := client.Read(buf)
			if err != nil {
				return
			}
			data := make([]byte, n)
			copy(data, buf[:n])
			output <- data
		}
	}()

	time.Sleep(100 * time.Millisecond)

	t.Log("calling clipboard/set via Janet (OSC52 path)...")

	done := make(chan error, 1)
	go func() {
		done <- client.execute(`(clipboard/set "hello-osc52")`)
	}()

	select {
	case err := <-done:
		require.NoError(t, err)
		t.Log("clipboard/set returned without error")
	case <-time.After(5 * time.Second):
		t.Fatal("clipboard/set timed out - likely blocked on pipe write")
	}

	// Drain output and look for OSC52 sequence
	t.Log("draining renderer output looking for OSC52...")
	deadline := time.After(2 * time.Second)
	found := false
	for !found {
		select {
		case data := <-output:
			t.Logf("received %d bytes from renderer", len(data))
			if containsOSC52(string(data)) {
				t.Log("found OSC52 sequence in output!")
				found = true
			}
		case <-deadline:
			t.Log("timed out waiting for OSC52 in output")
			goto doneOSC52
		}
	}
doneOSC52:
	require.True(t, found, "OSC52 sequence not found in renderer output")
}

func containsOSC52(s string) bool {
	return len(s) > 5 && containsSubstring(s, "\033]52;")
}

func containsSubstring(s, sub string) bool {
	for i := 0; i <= len(s)-len(sub); i++ {
		if s[i:i+len(sub)] == sub {
			return true
		}
	}
	return false
}

// TestClipboardViaRegister tests the register/set "+" path which is
// what replay copy actually uses.
func TestClipboardViaRegister(t *testing.T) {
	server, create, err := NewTestServer()
	require.NoError(t, err)
	defer server.Cancel()

	client, err := create(geom.DEFAULT_SIZE)
	require.NoError(t, err)
	defer client.Cancel()

	mc, ok := server.options.Clipboard.(*clipboard.MemoryClipboard)
	require.True(t, ok)

	t.Log("calling register/set '+' via Janet...")
	err = client.execute(`(register/set "+" "register-test")`)
	require.NoError(t, err)

	val, err := mc.Read()
	require.NoError(t, err)
	t.Log("MemoryClipboard value:", val)
	require.Equal(t, "register-test", val)

	err = client.execute(`(assert (= "register-test" (register/get "+")))`)
	require.NoError(t, err)
	t.Log("register/get '+' matches")
}

// TestClipboardDirectWrite tests writing directly to the clipboard
// object to isolate any issues in the Go layer.
func TestClipboardDirectWrite(t *testing.T) {
	mc := &clipboard.MemoryClipboard{}
	err := mc.Write("direct-test")
	require.NoError(t, err)
	val, err := mc.Read()
	require.NoError(t, err)
	require.Equal(t, "direct-test", val)

	// Test OSC52 clipboard with a buffer
	r, w := io.Pipe()
	osc := clipboard.NewOSC52Clipboard(w)

	received := make(chan string, 1)
	go func() {
		buf := make([]byte, 4096)
		n, _ := r.Read(buf)
		received <- string(buf[:n])
	}()

	err = osc.Write("osc52-test")
	require.NoError(t, err)

	select {
	case data := <-received:
		t.Log("OSC52 output:", data)
		require.Contains(t, data, "\033]52;c;")
	case <-time.After(2 * time.Second):
		t.Fatal("OSC52 write blocked")
	}
}

// TestClipboardCopyEventChain tests the full CopyEvent pipeline:
// a CopyEvent is published on a pane's screen, travels through the
// tree subscriber system, and is handled by pollNodeEvents to write
// to the clipboard. This simulates what happens when a user copies
// text in replay mode.
func TestClipboardCopyEventChain(t *testing.T) {
	server, create, err := NewTestServer()
	require.NoError(t, err)
	defer server.Cancel()

	client, err := create(geom.DEFAULT_SIZE)
	require.NoError(t, err)
	defer client.Cancel()

	mc, ok := server.options.Clipboard.(*clipboard.MemoryClipboard)
	require.True(t, ok)

	node := client.Node()
	require.NotNil(t, node)

	// Publish a CopyEvent directly on the pane's screen, simulating
	// what handleCopy does after reading the selected text.
	t.Log("publishing CopyEvent on pane screen...")
	t.Logf("  node ID: %d", node.Id())
	t.Logf("  node type: %T", node)

	screen := node.(*T.Pane).Screen()
	t.Logf("  screen type: %T", screen)

	r, ok := screen.(*replayable.Replayable)
	require.True(t, ok, "expected Replayable screen")

	// The CopyEvent with Text filled in is what gets published
	// after handleCopy processes the selection.
	r.Publish(replay.CopyEvent{
		Register: "+",
		Text:     "event-chain-test",
	})

	// Give pollNodeEvents time to process the event
	time.Sleep(500 * time.Millisecond)

	val, err := mc.Read()
	require.NoError(t, err)
	t.Logf("MemoryClipboard value after CopyEvent: %q", val)
	require.Equal(t, "event-chain-test", val,
		"CopyEvent did not reach clipboard - "+
			"event was likely dropped in publish chain")
}

// TestClipboardOSC52Passthrough tests that programs inside cy panes
// can write to the system clipboard via OSC 52 escape sequences.
func TestClipboardOSC52Passthrough(t *testing.T) {
	server, create, err := NewTestServer()
	require.NoError(t, err)
	defer server.Cancel()

	client, err := create(geom.DEFAULT_SIZE)
	require.NoError(t, err)
	defer client.Cancel()

	mc, ok := server.options.Clipboard.(*clipboard.MemoryClipboard)
	require.True(t, ok)

	node := client.Node()
	require.NotNil(t, node)

	pane := node.(*T.Pane)
	screen := pane.Screen()

	r, ok := screen.(*replayable.Replayable)
	require.True(t, ok)

	// Write an OSC 52 sequence to the terminal as if a program
	// (e.g., neovim) inside the pane wrote it. We must write to
	// the Terminal's Write() method (program output path), not
	// Send() which goes to stdin.
	// "hello-passthrough" base64 = "aGVsbG8tcGFzc3Rocm91Z2g="
	t.Log("writing OSC 52 to terminal...")
	terminal := r.Screen().(*S.Terminal)
	_, err = terminal.Write([]byte("\033]52;c;aGVsbG8tcGFzc3Rocm91Z2g=\007"))

	// Give the event chain time to propagate
	time.Sleep(500 * time.Millisecond)

	val, err := mc.Read()
	require.NoError(t, err)
	t.Logf("MemoryClipboard value after OSC 52: %q", val)
	require.Equal(t, "hello-passthrough", val,
		"OSC 52 passthrough did not reach clipboard")
}
