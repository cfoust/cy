package anim

import (
	"math"
	"math/rand"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
)

const (
	REFORM_CHARS  = "—~±§|[].+$^@*()•x%!?#"
	HEADER_HEIGHT = 6
	FOOTER_HEIGHT = 12
	PANEL_WIDTH   = 32
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

// Reform was inspired by the loading animation on
// https://musicforprogramming.net/.
type Reform struct {
	in, bg, out image.Image
	// Pre-rendered formatted text (to avoid rendering on every frame)
	header, footer image.Image
	duration       time.Duration
	words          []word
	render         *taro.Renderer
	lastReverse    bool
}

var _ Animation = (*Reform)(nil)

func (r *Reform) Init(start image.Image) {
	r.render = taro.NewRenderer()
	r.in = start.Clone()
	r.bg = start
	r.out = image.New(start.Size())

	// Find all of the sequences of non-blank characters
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

	re := r.render

	// Pre-render the header and footer
	r.header = image.New(geom.Vec2{
		R: HEADER_HEIGHT,
		C: PANEL_WIDTH,
	})

	re.RenderAt(
		r.header,
		0, 0,
		re.NewStyle().
			Foreground(lipgloss.Color("8")).
			Render("// musicforprogramming.net\n"),
	)

	headerText := "" +
		re.NewStyle().
			Foreground(lipgloss.Color("4")).
			Render("function") +
		" " +
		re.NewStyle().
			Foreground(lipgloss.Color("2")).
			Render("musicFor") +
		"(" +
		re.NewStyle().
			Foreground(lipgloss.Color("3")).
			Italic(true).
			Render("task") +
		" = " +
		re.NewStyle().
			Foreground(lipgloss.Color("3")).
			Render("'programming'") +
		") { " +
		re.NewStyle().
			Foreground(lipgloss.Color("5")).
			Render("return") +
		" " +
		re.NewStyle().
			Foreground(lipgloss.Color("13")).
			Render("`A series of mixes intended for listening while ") +
		re.NewStyle().
			Foreground(lipgloss.Color("3")).
			Render("${") +
		re.NewStyle().
			Foreground(lipgloss.Color("15")).
			Render("task") +
		re.NewStyle().
			Foreground(lipgloss.Color("3")).
			Render("} ") +
		re.NewStyle().
			Foreground(lipgloss.Color("13")).
			Render("to focus the brain and inspire the mind.`") +
		"; }"

	re.RenderAtSize(
		r.header,
		1, 0,
		HEADER_HEIGHT, PANEL_WIDTH,
		headerText,
	)

	r.footer = image.New(geom.Vec2{
		R: FOOTER_HEIGHT,
		C: PANEL_WIDTH,
	})

	footerText := "" +
		re.NewStyle().
			Foreground(lipgloss.Color("2")).
			Render("[prev] [-30] [stop] [+30] [next]") +
		"\n" +
		re.NewStyle().
			Foreground(lipgloss.Color("8")).
			Render("00:00:00 ") +
		re.NewStyle().
			Foreground(lipgloss.Color("13")).
			Render("[v+] ") +
		re.NewStyle().
			Foreground(lipgloss.Color("8")).
			Render("100% ") +
		re.NewStyle().
			Foreground(lipgloss.Color("13")).
			Render("[v-] [random]") +
		"\n" +
		"\n" +
		re.NewStyle().
			Foreground(lipgloss.Color("4")).
			Render("[About] [Credits] [RSS]") +
		"\n" +
		re.NewStyle().
			Foreground(lipgloss.Color("9")).
			Render("[Patreon] [YouTube]") +
		"\n" +
		re.NewStyle().
			Foreground(lipgloss.Color("5")).
			Render("[folder.jpg] [Enterprise]") +
		"\n" +
		re.NewStyle().
			Foreground(lipgloss.Color("13")).
			Render("[Invert] [Fullscreen]") +
		"\n" +
		"\n" +
		re.NewStyle().
			Foreground(lipgloss.Color("8")).
			Render(`// 72 episodes
// 1254 tracks
// 103 hours
// 57 minutes
// 11 seconds`)

	re.RenderAtSize(
		r.footer,
		0, 0,
		FOOTER_HEIGHT, PANEL_WIDTH,
		footerText,
	)
}

// getRandomCharacter returns a random character from a predefined set.
func getRandomCharacter(isXOnly bool) rune {
	specialChars := "—~±§|[].+$^@*()•x%!?#"
	if isXOnly {
		return 'x'
	}
	return rune(specialChars[rand.Intn(len(specialChars))])
}

func (r *Reform) drawBackground(delta time.Duration) image.Image {
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
					r.bg[w.row][w.col0+col].Char = ' '
					continue
				}

				if col < numReal {
					r.bg[w.row][w.col0+col].Char = w.buffer[col].Char
					continue
				}

				r.bg[w.row][w.col0+col].Char = r.in[w.row][w.col0+col].Char
			}
			continue
		}

		numRandom := geom.Min(charsToShow+randomReplaceRange, length-1)

		// Otherwise the order goes: real, random, empty
		for col := 0; col < length; col++ {
			if col < charsToShow {
				r.bg[w.row][w.col0+col].Char = r.in[w.row][w.col0+col].Char
				continue
			}

			if col < numRandom {
				r.bg[w.row][w.col0+col].Char = w.buffer[col].Char
				continue
			}

			r.bg[w.row][w.col0+col].Char = ' '
		}
	}

	image.Copy(geom.Vec2{}, r.out, r.bg)
	return r.bg
}

func (r *Reform) Update(delta time.Duration) image.Image {
	r.drawBackground(delta)

	image.Copy(geom.Vec2{
		R: 1,
		C: 1,
	}, r.out, r.header)

	image.Copy(geom.Vec2{
		R: 1 + HEADER_HEIGHT,
		C: 1,
	}, r.out, r.footer)

	return r.out
}

func init() {
	registerAnimation("reform", func() Animation {
		return &Reform{
			duration: time.Second + time.Millisecond*500,
		}
	})
}
