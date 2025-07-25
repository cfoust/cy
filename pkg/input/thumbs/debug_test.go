package thumbs

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/stretchr/testify/require"
)

func TestThumbsDebugSimple(t *testing.T) {
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
	
	test := taro.Test(thumbs)
	
	// Test basic rendering
	test(geom.Size{R: 5, C: 50})
	
	// Test escape - should exit quickly
	done := make(chan bool, 1)
	go func() {
		test("esc")
		done <- true
	}()
	
	select {
	case <-done:
		// Success - escape worked
	case <-time.After(5 * time.Second):
		t.Fatal("Escape key did not exit thumbs within 5 seconds")
	}
}