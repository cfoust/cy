package anim

import (
	"fmt"
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
	VISUAL_HEIGHT = 4
	FOOTER_HEIGHT = 12
	PANEL_WIDTH   = 32
)

var TRACK_TITLE = []rune("Episode 1337: cy, the time traveling terminal multiplexer • ")

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

// MFP was inspired by the loading animation on
// https://musicforprogramming.net/.
type MFP struct {
	in, bg, out image.Image
	// Pre-rendered formatted text (to avoid rendering on every frame)
	header, footer image.Image
	visualizer     image.Image
	duration       time.Duration
	words          []word
	render         *taro.Renderer
	lastReverse    bool
}

var _ Animation = (*MFP)(nil)

func (r *MFP) Init(start image.Image) {
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

	// Initialize the visualizer
	r.visualizer = image.New(geom.Vec2{
		R: VISUAL_HEIGHT,
		C: PANEL_WIDTH,
	})

	for col := 0; col < PANEL_WIDTH; col++ {
		r.visualizer[0][col].FG = emu.ANSIColor(5)
		r.visualizer[1][col].FG = emu.ANSIColor(9)
		r.visualizer[2][col].FG = emu.ANSIColor(11)
		r.visualizer[3][col].FG = emu.ANSIColor(2)
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

func (r *MFP) drawBackground(delta time.Duration) {
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
	pauseFactor := 0.8
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
}

func formatDuration(duration time.Duration) string {
	seconds := int(duration.Seconds())
	hours := seconds / 3600
	minutes := (seconds % 3600) / 60
	seconds = seconds % 60
	return fmt.Sprintf(
		"%02d:%02d:%02d",
		hours, minutes, seconds,
	)
}

var (
	VISUALIZER_0 = []rune("                       _.-•:*^º'")
	VISUALIZER_1 = []rune("               _.-•:*^º'        ")
	VISUALIZER_2 = []rune("       _.-•:*^º'                ")
	VISUALIZER_3 = []rune("_.-•:*^º'                       ")
)

func (r *MFP) drawVisualizer(delta time.Duration) {
	time := delta.Seconds()
	var offset int
	var factor float64
	for col := 0; col < PANEL_WIDTH; col++ {
		factor = (math.Sin(time*float64(col+1)*0.1) + 1) / 2
		offset = int(factor * 32)
		offset = geom.Clamp(offset, 0, 32)
		r.visualizer[0][col].Char = VISUALIZER_0[offset%len(VISUALIZER_0)]
		r.visualizer[1][col].Char = VISUALIZER_1[offset%len(VISUALIZER_1)]
		r.visualizer[2][col].Char = VISUALIZER_2[offset%len(VISUALIZER_2)]
		r.visualizer[3][col].Char = VISUALIZER_3[offset%len(VISUALIZER_3)]
	}
}

func (r *MFP) Update(delta time.Duration) image.Image {
	r.drawBackground(delta)
	image.Copy(geom.Vec2{}, r.out, r.bg)

	colOffset := 1
	rowOffset := 1

	for row := 0; row < r.out.Size().R; row++ {
		for col := 0; col < geom.Min(r.out.Size().C-1, PANEL_WIDTH+colOffset); col++ {
			r.out[row][col].Char = ' '
		}
	}

	image.Copy(geom.Vec2{
		R: rowOffset,
		C: colOffset,
	}, r.out, r.header)

	visualRow := HEADER_HEIGHT + rowOffset
	r.drawVisualizer(delta)
	image.Copy(geom.Vec2{
		R: visualRow,
		C: colOffset,
	}, r.out, r.visualizer)

	titleIndex := int(delta.Milliseconds() / 500)
	for col := 0; col < PANEL_WIDTH; col++ {
		cellRow := visualRow + VISUAL_HEIGHT
		cellCol := col + colOffset
		r.out[cellRow][cellCol].Char = TRACK_TITLE[(col+titleIndex)%len(TRACK_TITLE)]
		r.out[cellRow][cellCol].FG = emu.ANSIColor(8)
	}

	footerRow := visualRow + VISUAL_HEIGHT + 1
	image.Copy(geom.Vec2{
		R: footerRow,
		C: colOffset,
	}, r.out, r.footer)

	r.render.RenderAt(
		r.out,
		footerRow, colOffset,
		r.render.NewStyle().
			Foreground(lipgloss.Color("8")).
			Render(formatDuration(delta)),
	)

	return r.out
}

func init() {
	registerAnimation("musicforprogramming", func() Animation {
		return &MFP{
			duration: 5 * time.Second,
		}
	})
}
