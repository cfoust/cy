package anim

import (
	"math"
	"math/rand"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
)

const (
	REFORM_CHARS = "—~±§|[].+$^@*()•x%!?#"
)

type word struct {
	row    int
	col0   int
	col1   int // exclusive
	buffer []emu.Glyph
}

func makeBuffer(size int) []emu.Glyph {
	buffer := make([]emu.Glyph, size)
	for i := 0; i < size; i++ {
		buffer[i] = emu.EmptyGlyph()
	}
	return buffer
}

type Reform struct {
	in          image.Image
	out         image.Image
	duration    time.Duration
	words       []word
	lastReverse bool
}

var _ Animation = (*Reform)(nil)

func (r *Reform) Init(start image.Image) {
	r.in = start.Clone()
	r.out = start

	for row := 0; row < start.Size().R; row++ {
		var col0 int = -1
		var first emu.Glyph

		for col := 0; col < start.Size().C; col++ {
			if start[row][col].Char == ' ' || (col0 != -1 && !start[row][col].SameAttrs(first)) {
				if col0 == -1 {
					continue
				}

				r.words = append(r.words, word{
					row:    row,
					col0:   col0,
					col1:   col,
					buffer: makeBuffer(col - col0),
				})
				col0 = -1
				continue
			}

			if col0 != -1 {
				continue
			}

			col0 = col
			first = start[row][col]
		}

		if col0 == -1 {
			continue
		}

		r.words = append(r.words, word{
			row:    row,
			col0:   col0,
			col1:   start.Size().C,
			buffer: makeBuffer(start.Size().C - col0),
		})
	}
}

// getRandomCharacter returns a random character from a predefined set.
func getRandomCharacter(isXOnly bool) rune {
	specialChars := "—~±§|[].+$^@*()•x%!?#"
	if isXOnly {
		return 'x'
	}
	return rune(specialChars[rand.Intn(len(specialChars))])
}

func (r *Reform) Update(delta time.Duration) image.Image {
	cycle := (delta / r.duration)
	reverse := (cycle % 2) == 1

	randomCharChance := 0.8
	if reverse {
		randomCharChance = 0.1
	}

	if reverse != r.lastReverse {
		for _, w := range r.words {
			for col := 0; col < w.col1-w.col0; col++ {
				w.buffer[col].Char = ' '
			}
		}
	}

	r.lastReverse = reverse

	// Pause at the end of each cycle
	pauseFactor := 0.2
	pauseLength := time.Duration(float64(r.duration) * pauseFactor)
	progress := float64(float64(delta-(cycle*r.duration))) / float64(r.duration-pauseLength)

	if progress > 1 {
		progress = 1
	}

	// Easing function for smooth animation
	easedProgress := -(math.Cos(math.Pi*progress) - 1) / 2
	easedProgress = math.Pow(easedProgress, 2)
	if reverse {
		easedProgress = 1 - easedProgress
	}

	for _, w := range r.words {
		length := w.col1 - w.col0
		wordProgress := float64(length) * math.Abs(easedProgress)
		charsToShow := int(math.Floor(wordProgress))
		if reverse {
			charsToShow = int(math.Floor(wordProgress))
		}

		randomReplaceRange := int(2 * (0.5 - math.Abs(easedProgress-0.5)) * float64(length))

		// Update the temporary text with new characters
		for j := 0; j < 20; j++ {
			pos := charsToShow + int((1-rand.Float64())*float64(length)*float64(j)/20)
			if pos < 0 || pos >= len(w.buffer) {
				continue
			}

			if rand.Float64() > randomCharChance {
				w.buffer[pos].Char = r.in[w.row][w.col0+pos].Char
			} else {
				w.buffer[pos].Char = getRandomCharacter(reverse)
			}
		}

		if reverse {
			numRandom := geom.Max(0, charsToShow-randomReplaceRange)
			numReal := geom.Max(0, charsToShow)

			// In reverse the order goes: empty, random, real
			for col := 0; col < length; col++ {
				if col < numRandom {
					r.out[w.row][w.col0+col].Char = ' '
					continue
				}

				if col < numReal {
					r.out[w.row][w.col0+col].Char = w.buffer[col].Char
					continue
				}

				r.out[w.row][w.col0+col].Char = r.in[w.row][w.col0+col].Char
			}
			continue
		}

		numRandom := geom.Min(charsToShow+randomReplaceRange, length-1)

		// Otherwise the order goes: real, random, empty
		for col := 0; col < length; col++ {
			if col < charsToShow {
				r.out[w.row][w.col0+col].Char = r.in[w.row][w.col0+col].Char
				continue
			}

			if col < numRandom {
				r.out[w.row][w.col0+col].Char = w.buffer[col].Char
				continue
			}

			r.out[w.row][w.col0+col].Char = ' '
		}
	}

	return r.out
}

func init() {
	registerAnimation("mfp-reform", func() Animation {
		return &Reform{
			duration: time.Second + time.Millisecond*500,
		}
	})
}
