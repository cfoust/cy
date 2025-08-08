package thumbs

import (
	"context"
	"fmt"
	"testing"

	"github.com/cfoust/cy/pkg/taro"

	"github.com/stretchr/testify/require"
)

func TestThumbsUpdateDebug(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	screenLines := []string{
		"Simple test with https://example.com",
	}
	
	thumbs := newThumbs(ctx, screenLines, nil)
	
	// Debug: Print matches found
	fmt.Printf("Found %d matches:\n", len(thumbs.matches))
	for i, match := range thumbs.matches {
		fmt.Printf("  [%d] hint='%s' text='%s' pattern='%s' pos=(%d,%d)\n", 
			i, match.Hint, match.Text, match.Pattern, match.X, match.Y)
	}
	
	require.Greater(t, len(thumbs.matches), 0, "Should find at least one match")

	// Test escape key directly
	escKeyMsgs := taro.KeysToMsg("esc")
	require.Len(t, escKeyMsgs, 1, "Should get exactly one escape key message")
	
	escKey := escKeyMsgs[0]
	fmt.Printf("Escape key: Type=%v, Alt=%v, Runes=%v\n", escKey.Type, escKey.Alt, escKey.Runes)
	
	// Call Update directly with the escape key
	model, cmd := thumbs.Update(escKey)
	fmt.Printf("After escape: model type=%T, cmd=%v\n", model, cmd != nil)
	
	// The result should be sent to the channel if it's set
	if thumbs.result == nil {
		fmt.Printf("No result channel set\n")
	}
}