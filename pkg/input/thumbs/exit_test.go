package thumbs

import (
	"context"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/taro"

	"github.com/stretchr/testify/require"
)

func TestThumbsExitBehavior(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	screenLines := []string{
		"Simple test with https://example.com",
	}

	// Create a result channel
	result := make(chan interface{}, 1)
	
	thumbs := newThumbs(ctx, screenLines, nil, WithResult(result))
	
	// Verify matches were found
	require.Greater(t, len(thumbs.matches), 0, "Should find at least one match")

	// Test escape key should send nil to result channel
	escKeyMsgs := taro.KeysToMsg("esc")
	require.Len(t, escKeyMsgs, 1, "Should get exactly one escape key message")
	
	escKey := escKeyMsgs[0]
	
	// Call Update with the escape key
	_, cmd := thumbs.Update(escKey)
	require.NotNil(t, cmd, "Update should return a command")

	// Execute the command to trigger the quit behavior
	if cmd != nil {
		cmd()
	}

	// Check that nil was sent to the result channel (indicating cancellation)
	select {
	case val := <-result:
		require.Nil(t, val, "Escape should send nil to result channel")
	case <-time.After(1 * time.Second):
		t.Fatal("Expected result channel to receive a value within 1 second")
	}
}

func TestThumbsMatchSelection(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	screenLines := []string{
		"Simple test with https://example.com",
	}

	// Create a result channel
	result := make(chan interface{}, 1)
	
	thumbs := newThumbs(ctx, screenLines, nil, WithResult(result))
	
	// Verify matches were found
	require.Greater(t, len(thumbs.matches), 0, "Should find at least one match")
	
	firstMatch := thumbs.matches[0]
	require.NotEmpty(t, firstMatch.Hint, "First match should have a hint")

	// Type the hint characters to select the match
	for _, char := range firstMatch.Hint {
		keyMsgs := taro.KeysToMsg(string(char))
		require.Len(t, keyMsgs, 1, "Should get exactly one key message")
		
		_, cmd := thumbs.Update(keyMsgs[0])
		if cmd != nil {
			cmd()
		}
	}

	// Check that the match text was sent to the result channel
	select {
	case val := <-result:
		require.Equal(t, firstMatch.Text, val, "Should receive the matched text")
	case <-time.After(1 * time.Second):
		t.Fatal("Expected result channel to receive a value within 1 second")
	}
}