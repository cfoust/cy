package thumbs

import (
	"context"
	"testing"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/stretchr/testify/require"
)

// Helper function to create mock screen content with text patterns
func createTestScreenLines() []string {
	return []string{
		"Welcome to cy terminal multiplexer",
		"Visit https://github.com/cfoust/cy for more info",
		"Check out https://docs.cy.rs for documentation",
		"File paths: /var/log/nginx.log and /tmp/test.txt",
		"IP addresses: 127.0.0.1 and 192.168.1.100",
		"Git SHA: a1b2c3d4e5f6 and full hash 973113963b491874ab2e372ee60d4b4cb75f717c",
		"UUID: 123e4567-e89b-12d3-a456-426655440000",
		"Colors: #ff0000, #00ff00, #0000ff",
		"Numbers: 1234, 5678, 9999",
		"Docker: sha256:30557a29d5abc51e5f1d5b472e79b7e296f595abcf19fe6b9199dbbc809c6ff4",
	}
}

func TestThumbsBasicSelection(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	screenLines := createTestScreenLines()
	thumbs := newThumbs(ctx, screenLines, nil)
	
	// Should find matches and generate hints
	require.Greater(t, len(thumbs.matches), 0, "Should find matches")
	
	test := taro.Test(thumbs)
	
	// Test basic rendering
	test(geom.Size{R: 15, C: 80})
	
	// Simulate typing the first hint to select it
	if len(thumbs.matches) > 0 {
		firstHint := thumbs.matches[0].Hint
		test(firstHint)
		// The thumbs should exit after selection, so we can't test further state
	}
}

func TestThumbsKeyboardNavigation(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	screenLines := createTestScreenLines()
	thumbs := newThumbs(ctx, screenLines, nil)
	
	require.Greater(t, len(thumbs.matches), 0, "Should find matches")
	
	test := taro.Test(thumbs)
	test(geom.Size{R: 15, C: 80})
	
	// Test escape key
	test("esc")
	// Should exit after escape
}

func TestThumbsPartialHints(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Create content that will generate multi-character hints
	var screenLines []string
	for i := 0; i < 30; i++ {
		screenLines = append(screenLines, 
			"URL: https://example.com/path"+string(rune('a'+i%26)))
	}
	
	thumbs := newThumbs(ctx, screenLines, nil)
	require.Greater(t, len(thumbs.matches), 26, "Should have enough matches for multi-char hints")
	
	test := taro.Test(thumbs)
	test(geom.Size{R: 25, C: 80})
	
	// Find a multi-character hint
	var multiCharHint string
	for _, match := range thumbs.matches {
		if len(match.Hint) > 1 {
			multiCharHint = match.Hint
			break
		}
	}
	
	if multiCharHint != "" {
		// Test typing partial hint - should not select yet
		test(string(multiCharHint[0]))
		// Should still be active, waiting for more input
		
		// Complete the hint
		test(string(multiCharHint[1]))
		// Should select and exit
	}
}

func TestThumbsCustomAlphabet(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	screenLines := []string{
		"URL: https://example.com",
		"Path: /tmp/test.txt",
		"IP: 192.168.1.1",
	}
	
	customAlphabet := "123456789"
	thumbs := newThumbs(ctx, screenLines, nil, WithAlphabet(customAlphabet))
	
	require.Greater(t, len(thumbs.matches), 0, "Should find matches")
	
	// Verify hints use custom alphabet
	for _, match := range thumbs.matches {
		for _, char := range match.Hint {
			require.Contains(t, customAlphabet, string(char), 
				"Hint should use custom alphabet")
		}
	}
	
	test := taro.Test(thumbs)
	test(geom.Size{R: 10, C: 50})
	
	// Test selecting with numeric hint
	if len(thumbs.matches) > 0 {
		firstHint := thumbs.matches[0].Hint
		test(firstHint)
	}
}

func TestThumbsCustomPatterns(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	screenLines := []string{
		"Email: user@example.com and admin@test.org",
		"MAC: 01:23:45:67:89:ab and 02:34:56:78:9a:bc",
		"Phone: +1-555-123-4567",
	}
	
	customPatterns := []string{
		`[\w\.-]+@[\w\.-]+\.\w+`,                                       // Email
		`[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}`, // MAC
		`\+?[0-9]+-[0-9]+-[0-9]+-[0-9]+`,                              // Phone
	}
	
	thumbs := newThumbs(ctx, screenLines, customPatterns)
	
	// Should find custom pattern matches
	require.Greater(t, len(thumbs.matches), 0, "Should find custom pattern matches")
	
	// Verify we found some custom matches
	foundCustom := false
	for _, match := range thumbs.matches {
		if match.Pattern == "custom" {
			foundCustom = true
			break
		}
	}
	require.True(t, foundCustom, "Should find at least one custom pattern match")
	
	test := taro.Test(thumbs)
	test(geom.Size{R: 8, C: 60})
	
	// Test selection
	if len(thumbs.matches) > 0 {
		firstHint := thumbs.matches[0].Hint
		test(firstHint)
	}
}

func TestThumbsInvalidInput(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	screenLines := []string{
		"Simple text with https://example.com",
	}
	
	thumbs := newThumbs(ctx, screenLines, nil)
	require.Greater(t, len(thumbs.matches), 0, "Should find matches")
	
	test := taro.Test(thumbs)
	test(geom.Size{R: 5, C: 40})
	
	// Test invalid character that doesn't match any hint
	test("z") // Assuming 'z' is not in the first few hints
	// Should still be active, invalid input ignored
	
	// Test valid hint after invalid input
	if len(thumbs.matches) > 0 {
		firstHint := thumbs.matches[0].Hint
		test(firstHint)
	}
}

func TestThumbsWithInitialImage(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	screenLines := []string{
		"Test content with https://example.com",
		"And a file path /tmp/test.txt",
	}
	
	// Create a mock initial image
	initialSize := geom.Vec2{R: 5, C: 40}
	initial := image.New(initialSize)
	
	thumbs := newThumbs(ctx, screenLines, nil, WithInitial(initial))
	require.Greater(t, len(thumbs.matches), 0, "Should find matches")
	
	test := taro.Test(thumbs)
	test(geom.Size{R: 5, C: 40})
	
	// Test that it renders without crashing
	if len(thumbs.matches) > 0 {
		firstHint := thumbs.matches[0].Hint
		test(firstHint)
	}
}

func TestThumbsSmallScreen(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	screenLines := []string{
		"https://example.com",
	}
	
	thumbs := newThumbs(ctx, screenLines, nil)
	test := taro.Test(thumbs)
	
	// Test very small screen - should not crash
	test(geom.Size{R: 3, C: 10})
	
	// Should still be able to handle input
	test("esc")
}

func TestThumbsEnterKey(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	screenLines := []string{
		"Test with https://example.com and /tmp/file.txt",
	}
	
	thumbs := newThumbs(ctx, screenLines, nil)
	require.Greater(t, len(thumbs.matches), 0, "Should find matches")
	
	test := taro.Test(thumbs)
	test(geom.Size{R: 5, C: 50})
	
	// Type partial hint and press enter - should not select if partial is invalid
	test("x", "enter") // Assuming 'x' is not a complete hint
	
	// Type valid hint and enter
	if len(thumbs.matches) > 0 {
		firstHint := thumbs.matches[0].Hint
		test(firstHint, "enter")
	}
}

func TestThumbsControlC(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	screenLines := []string{
		"Test content with patterns https://example.com",
	}
	
	thumbs := newThumbs(ctx, screenLines, nil)
	test := taro.Test(thumbs)
	test(geom.Size{R: 5, C: 50})
	
	// Test ctrl+c cancellation
	test("ctrl+c")
	// Should exit
}

func TestThumbsNoMatches(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Content with no recognizable patterns
	screenLines := []string{
		"Just plain text",
		"Nothing interesting here",
		"No URLs or paths",
	}
	
	thumbs := newThumbs(ctx, screenLines, nil)
	
	// Should handle case with no matches gracefully
	test := taro.Test(thumbs)
	test(geom.Size{R: 5, C: 30})
	
	// Should still respond to escape
	test("esc")
}