package thumbs

import (
	"context"
	"fmt"
	"strings"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/stories"
)

func createMockScreen(content string) image.Image {
	lines := strings.Split(content, "\n")
	maxCols := 0
	for _, line := range lines {
		if len(line) > maxCols {
			maxCols = len(line)
		}
	}

	size := geom.Vec2{R: len(lines), C: maxCols}
	img := image.New(size)

	// Clear the entire image
	img.Clear(geom.Rect{
		Position: geom.Vec2{},
		Size:     size,
	})

	for r, line := range lines {
		for c, char := range line {
			if c >= size.C {
				break
			}
			glyph := emu.Glyph{
				Char: char,
				FG:   emu.DefaultFG,
				BG:   emu.DefaultBG,
			}
			img[r][c] = glyph
		}
	}

	return img
}

func init() {
	stories.Register(
		"input/thumbs/basic",
		func(ctx context.Context) (mux.Screen, error) {
			content := `Welcome to cy terminal multiplexer
Visit https://github.com/cfoust/cy for more info
Check out the documentation at https://docs.cy.rs
File paths: /var/log/nginx.log and /tmp/test.txt
IP addresses: 127.0.0.1 and 192.168.1.100
Git SHA: a1b2c3d4e5f6 and full hash 973113963b491874ab2e372ee60d4b4cb75f717c
UUID: 123e4567-e89b-12d3-a456-426655440000
Colors: #ff0000, #00ff00, #0000ff
Docker: sha256:30557a29d5abc51e5f1d5b472e79b7e296f595abcf19fe6b9199dbbc809c6ff4
Numbers: 1234, 5678, 9999, 12345`

			screenLines := strings.Split(content, "\n")
			initial := createMockScreen(content)

			return New(
				ctx,
				screenLines,
				nil, // no custom patterns
				WithInitial(initial),
			), nil
		},
		stories.Config{
			Size: geom.Vec2{R: 15, C: 80},
			Input: []interface{}{
				stories.Wait(stories.Some),
				stories.Type("a"), // Select first match
			},
		},
	)

	stories.Register(
		"input/thumbs/custom-patterns",
		func(ctx context.Context) (mux.Screen, error) {
			content := `Email addresses in this text:
admin@example.com and user@test.org
Also some MAC addresses:
01:23:45:67:89:ab and 02:34:56:78:9a:bc
Phone numbers: +1-555-123-4567
Regular expressions are powerful!`

			screenLines := strings.Split(content, "\n")
			initial := createMockScreen(content)

			// Custom patterns for emails, MAC addresses, and phone numbers
			customPatterns := []string{
				`[\w\.-]+@[\w\.-]+\.\w+`, // Email
				`[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}`, // MAC
				`\+?[0-9]+-[0-9]+-[0-9]+-[0-9]+`,                                          // Phone
			}

			return New(
				ctx,
				screenLines,
				customPatterns,
				WithInitial(initial),
			), nil
		},
		stories.Config{
			Size: geom.Vec2{R: 10, C: 60},
			Input: []interface{}{
				stories.Wait(stories.Some),
				stories.Type("s"), // Try second match
			},
		},
	)

	stories.Register(
		"input/thumbs/custom-alphabet",
		func(ctx context.Context) (mux.Screen, error) {
			content := `URLs with numeric hints:
https://github.com/cfoust/cy
https://docs.cy.rs
https://www.rust-lang.org
File: /home/user/config.yaml
Path: /etc/nginx/nginx.conf`

			screenLines := strings.Split(content, "\n")
			initial := createMockScreen(content)

			return New(
				ctx,
				screenLines,
				nil,
				WithInitial(initial),
				WithAlphabet("123456789"), // Numeric alphabet
			), nil
		},
		stories.Config{
			Size: geom.Vec2{R: 8, C: 50},
			Input: []interface{}{
				stories.Wait(stories.Some),
				stories.Type("1"), // Select first match with numeric hint
			},
		},
	)

	stories.Register(
		"input/thumbs/markdown-urls",
		func(ctx context.Context) (mux.Screen, error) {
			content := `# Documentation Links

Check out [cy homepage](https://github.com/cfoust/cy) for more info.
Read the [documentation](https://docs.cy.rs) to get started.
See the [Rust website](https://www.rust-lang.org) for Rust info.

![Logo](https://github.com/cfoust/cy/logo.png)

Visit [multiple](https://example.com) [links](https://test.com) here.`

			screenLines := strings.Split(content, "\n")
			initial := createMockScreen(content)

			return New(
				ctx,
				screenLines,
				nil,
				WithInitial(initial),
			), nil
		},
		stories.Config{
			Size: geom.Vec2{R: 12, C: 70},
			Input: []interface{}{
				stories.Wait(stories.Some),
				stories.Type("d"), // Try fourth match
			},
		},
	)

	stories.Register(
		"input/thumbs/code-diff",
		func(ctx context.Context) (mux.Screen, error) {
			content := `diff --git a/src/main.rs b/src/main.rs
index 1234567..abcdefg 100644
--- a/src/main.rs
+++ b/src/main.rs
@@ -1,5 +1,6 @@
 fn main() {
     println!("Hello, world!");
+    println!("Modified file");
 }
 
commit fd70b5695123456789abcdef
Author: user@example.com`

			screenLines := strings.Split(content, "\n")
			initial := createMockScreen(content)

			return New(
				ctx,
				screenLines,
				nil,
				WithInitial(initial),
			), nil
		},
		stories.Config{
			Size: geom.Vec2{R: 15, C: 60},
			Input: []interface{}{
				stories.Wait(stories.Some),
				stories.Type("f"), // Try to select a file path
			},
		},
	)

	stories.Register(
		"input/thumbs/multi-char-hints",
		func(ctx context.Context) (mux.Screen, error) {
			// Create content with many matches to force multi-character hints
			var lines []string
			lines = append(lines, "Many URLs to demonstrate multi-character hints:")

			for i := 0; i < 30; i++ {
				lines = append(lines, fmt.Sprintf("URL %02d: https://example%02d.com/path%02d", i+1, i+1, i+1))
			}

			content := strings.Join(lines, "\n")
			screenLines := strings.Split(content, "\n")
			initial := createMockScreen(content)

			return New(
				ctx,
				screenLines,
				nil,
				WithInitial(initial),
			), nil
		},
		stories.Config{
			Size: geom.Vec2{R: 20, C: 80},
			Input: []interface{}{
				stories.Wait(stories.Some),
				stories.Type("aa"), // Multi-character hint
			},
		},
	)
}
