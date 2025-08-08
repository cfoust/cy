package thumbs

import (
	"strings"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
)

func (t *Thumbs) View(state *tty.State) {
	if state.Image.Size().IsZero() {
		return
	}

	t.size = state.Image.Size()

	// Start with the background - either animation or initial screen
	if t.anim != nil {
		// Copy animation state as background
		animState := t.anim.State()
		if animState != nil {
			image.Copy(geom.Vec2{}, state.Image, animState.Image)
		}
	} else if t.initial != nil {
		// Copy the initial image to the state
		image.Copy(geom.Vec2{}, state.Image, t.initial)
	}

	// Get the current input to determine which hints to highlight
	currentInput := strings.TrimSpace(t.textInput.Value())

	// Overlay hints on the screen
	for _, match := range t.matches {
		if match.Y >= t.size.R || match.X >= t.size.C {
			continue
		}

		// Determine if this hint should be highlighted
		isValidPrefix := strings.HasPrefix(match.Hint, currentInput)
		if currentInput == "" || isValidPrefix {
			// Show the hint
			hintText := match.Hint
			if currentInput != "" && isValidPrefix {
				// Highlight the matching prefix differently
				remaining := hintText[len(currentInput):]
				hintText = currentInput + remaining
			}

			// Calculate hint position - place it at the beginning of the match
			hintX := match.X
			hintY := match.Y

			// Ensure hint fits on screen
			if hintX+len(match.Hint) > t.size.C {
				hintX = t.size.C - len(match.Hint)
			}
			if hintX < 0 {
				hintX = 0
			}

			// Render the hint directly on the image
			for i, r := range hintText {
				if hintX+i >= t.size.C {
					break
				}

				// Create hint glyph with yellow foreground on black background
				glyph := emu.Glyph{
					Char: r,
					FG:   emu.LightYellow, // Yellow
					BG:   emu.Black,       // Black
				}
				state.Image[hintY][hintX+i] = glyph
			}
		}
	}

	// Show input prompt at the bottom
	if len(t.matches) > 0 {
		promptText := " Thumbs: " + t.textInput.Value()
		promptY := t.size.R - 1
		if promptY >= 0 && promptY < len(state.Image) {
			// Render prompt with blue background
			for i, r := range promptText {
				if i >= t.size.C {
					break
				}
				glyph := emu.Glyph{
					Char: r,
					FG:   emu.White, // White
					BG:   emu.Blue,  // Blue
				}
				state.Image[promptY][i] = glyph
			}
		}
	}

}
