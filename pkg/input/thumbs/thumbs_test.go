package thumbs

import (
	"context"
	"testing"

	"github.com/cfoust/cy/pkg/params"
	"github.com/stretchr/testify/require"
)

func TestFindMatches(t *testing.T) {
	ctx := context.Background()
	screenLines := []string{
		"Lorem ipsum https://www.rust-lang.org/tools lorem",
		"Lorem ipsumhttps://crates.io lorem https://github.io?foo=bar lorem ssh://github.io",
		"Path: /var/log/nginx.log and /tmp/test.txt",
		"IP addresses: 127.0.0.1 and 255.255.255.255",
		"Git SHA: fd70b5695 and longer hash 973113963b491874ab2e372ee60d4b4cb75f717c",
		"UUID: 123e4567-e89b-12d3-a456-426655440000",
		"Color: #ff0000 and #00ff00",
		"Email: user@example.com (custom pattern)",
	}

	// Test with default patterns only
	thumbs := newThumbs(ctx, screenLines, nil)
	matches := thumbs.matches

	// Should find multiple matches
	require.Greater(t, len(matches), 10, "Should find multiple matches")

	// Check for specific patterns
	foundPatterns := make(map[string]bool)
	foundTexts := make(map[string]bool)

	for _, match := range matches {
		foundPatterns[match.Pattern] = true
		foundTexts[match.Text] = true
	}

	// Verify URL matches
	require.True(t, foundPatterns["url"], "Should find URL pattern")
	require.True(t, foundTexts["https://www.rust-lang.org/tools"], "Should find rust-lang URL")
	require.True(t, foundTexts["https://crates.io"], "Should find crates.io URL")
	require.True(t, foundTexts["ssh://github.io"], "Should find SSH URL")

	// Verify path matches
	require.True(t, foundPatterns["path"], "Should find path pattern")
	require.True(t, foundTexts["/var/log/nginx.log"], "Should find nginx log path")
	require.True(t, foundTexts["/tmp/test.txt"], "Should find tmp file path")

	// Verify IP matches
	require.True(t, foundPatterns["ip"], "Should find IP pattern")
	require.True(t, foundTexts["127.0.0.1"], "Should find localhost IP")
	require.True(t, foundTexts["255.255.255.255"], "Should find broadcast IP")

	// Verify SHA matches
	require.True(t, foundPatterns["sha"], "Should find SHA pattern")
	require.True(t, foundTexts["fd70b5695"], "Should find short SHA")
	require.True(t, foundTexts["973113963b491874ab2e372ee60d4b4cb75f717c"], "Should find long SHA")

	// Verify UUID matches
	require.True(t, foundPatterns["uid"], "Should find UUID pattern")
	require.True(t, foundTexts["123e4567-e89b-12d3-a456-426655440000"], "Should find UUID")

	// Verify color matches
	require.True(t, foundPatterns["color"], "Should find color pattern")
	require.True(t, foundTexts["#ff0000"], "Should find red color")
	require.True(t, foundTexts["#00ff00"], "Should find green color")
}

func TestFindMatchesWithCustomPatterns(t *testing.T) {
	ctx := context.Background()
	screenLines := []string{
		"Email: user@example.com and admin@test.org",
		"MAC: 01:23:45:67:89:ab and 02:34:56:78:9a:bc",
	}

	// Test with custom patterns
	customPatterns := []string{
		`[\w\.-]+@[\w\.-]+\.\w+`, // Email pattern
		`[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}`, // MAC address pattern
	}

	thumbs := newThumbs(ctx, screenLines, customPatterns)
	matches := thumbs.matches

	// Should find custom matches
	require.Greater(t, len(matches), 0, "Should find custom matches")

	foundTexts := make(map[string]bool)
	foundCustom := false
	for _, match := range matches {
		foundTexts[match.Text] = true
		if match.Pattern == "custom" {
			foundCustom = true
		}
	}
	require.True(t, foundCustom, "Should find at least one custom pattern match")

	// Verify email matches (custom patterns have priority)
	require.True(t, foundTexts["user@example.com"], "Should find first email")
	require.True(t, foundTexts["admin@test.org"], "Should find second email")

	// Verify MAC address matches
	require.True(t, foundTexts["01:23:45:67:89:ab"], "Should find first MAC")
	require.True(t, foundTexts["02:34:56:78:9a:bc"], "Should find second MAC")
}

func TestGenerateHints(t *testing.T) {
	ctx := context.Background()
	thumbs := newThumbs(ctx, []string{}, nil)

	// Test hint generation for different numbers of matches
	testCases := []struct {
		numMatches int
		expected   int
	}{
		{1, 1},
		{5, 5},
		{26, 26}, // One full alphabet
		{27, 27}, // Should generate 'aa' for 27th
		{100, 100},
	}

	for _, tc := range testCases {
		hints := thumbs.generateHints(tc.numMatches)
		require.Equal(t, tc.expected, len(hints), "Should generate correct number of hints")

		// Check that all hints are unique
		hintSet := make(map[string]bool)
		for _, hint := range hints {
			require.False(t, hintSet[hint], "Hint %s should be unique", hint)
			hintSet[hint] = true
			require.Greater(t, len(hint), 0, "Hint should not be empty")
		}
	}

	// Test specific hint values for first few matches
	hints := thumbs.generateHints(5)
	require.Equal(t, "a", hints[0], "First hint should be 'a'")
	require.Equal(t, "s", hints[1], "Second hint should be 's'")
	require.Equal(t, "d", hints[2], "Third hint should be 'd'")
}

func TestCustomAlphabet(t *testing.T) {
	ctx := context.Background()
	screenLines := []string{"test line with some text"}

	customAlphabet := "123456789"
	thumbs := newThumbs(ctx, screenLines, nil, WithAlphabet(customAlphabet))

	if len(thumbs.matches) > 0 {
		hints := thumbs.generateHints(3)
		require.Equal(t, "1", hints[0], "First hint should use custom alphabet")
		require.Equal(t, "2", hints[1], "Second hint should use custom alphabet")
		require.Equal(t, "3", hints[2], "Third hint should use custom alphabet")
	}
}

func TestSettings(t *testing.T) {
	ctx := context.Background()
	screenLines := []string{"test"}

	// Test WithParams
	params := params.New()
	thumbs := newThumbs(ctx, screenLines, nil, WithParams(params))
	require.Equal(t, params, thumbs.params, "Should set params correctly")

	// Test WithAlphabet
	customAlphabet := "abcdef"
	thumbs = newThumbs(ctx, screenLines, nil, WithAlphabet(customAlphabet))
	require.Equal(t, []rune(customAlphabet), thumbs.alphabet, "Should set custom alphabet")

	// Test WithResult
	result := make(chan interface{}, 1)
	thumbs = newThumbs(ctx, screenLines, nil, WithResult(result))
	require.NotNil(t, thumbs.result, "Should set result channel")
}
