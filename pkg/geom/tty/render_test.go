package tty

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"

	"github.com/charmbracelet/lipgloss"
	"github.com/muesli/termenv"
	"github.com/stretchr/testify/require"
)

func testBytes(
	t *testing.T,
	name string,
	bytes []byte,
) {
	termA := emu.New()
	_, _ = termA.Write(bytes)

	termB := emu.New()
	newBytes := swapImage(
		termB.Screen(),
		termA.Screen(),
	)
	_, _ = termB.Write(newBytes)

	require.Equal(
		t,
		termA.Screen(),
		termB.Screen(),
		"style %s was not equal: '%#v' '%#v'",
		name,
		string(bytes),
		string(newBytes),
	)
}

func TestAttributes(t *testing.T) {
	r := lipgloss.NewRenderer(emu.New())
	r.SetColorProfile(termenv.TrueColor)

	for name, style := range map[string]lipgloss.Style{
		"blink": r.NewStyle().Blink(true),
		"bold":  r.NewStyle().Bold(true),
		"fg":    r.NewStyle().Foreground(lipgloss.Color("#123456")),
		"bg":    r.NewStyle().Background(lipgloss.Color("#123456")),
		"fg + bg": r.NewStyle().
			Background(lipgloss.Color("#123456")).
			Foreground(lipgloss.Color("#123456")),
		"italics":       r.NewStyle().Italic(true),
		"strikethrough": r.NewStyle().Strikethrough(true),
		"bg 255":        r.NewStyle().Foreground(lipgloss.Color("255")),
	} {
		testBytes(t, name, []byte(style.Render("on")+" off"))
	}

	testBytes(
		t,
		"style",
		[]byte(
			"\033[48;2;255;0;0m           \033[0m\033[3;38;2;0;0;255;48;2;255;0;0mtest\033[0m",
		),
	)

	// New SGR attributes
	for name, input := range map[string]string{
		"dim":      "\033[2mtext\033[22m rest",
		"hidden":   "\033[8mtext\033[28m rest",
		"overline": "\033[53mtext\033[55m rest",
	} {
		testBytes(t, name, []byte(input))
	}

	// OSC 8 hyperlinks
	for name, input := range map[string]string{
		"hyperlink":         "\033]8;;https://example.com\033\\link\033]8;;\033\\ rest",
		"hyperlink-with-id": "\033]8;id=foo;https://example.com\033\\link\033]8;;\033\\ rest",
	} {
		testBytes(t, name, []byte(input))
	}

	// Styled underlines
	for name, input := range map[string]string{
		"underline-single":           "\033[4mtext\033[24m rest",
		"underline-double":           "\033[4:2mtext\033[24m rest",
		"underline-curly":            "\033[4:3mtext\033[24m rest",
		"underline-dotted":           "\033[4:4mtext\033[24m rest",
		"underline-dashed":           "\033[4:5mtext\033[24m rest",
		"underline-color":            "\033[4:3;58;2;255;0;0mtext\033[0m rest",
		"underline-color-reset":      "\033[4:3;58;2;255;0;0mred\033[59m default\033[0m rest",
		"underline-color-then-plain": "\033[4:3;58;2;255;0;0mcolored\033[0m \033[4mplain\033[0m rest",
	} {
		testBytes(t, name, []byte(input))
	}
}

func TestCursorColor(t *testing.T) {
	size := emu.New().Size()

	// No cursor color change -> no OSC 12 emitted
	src := New(size)
	dst := New(size)
	output := Swap(dst, src)
	require.NotContains(t, string(output), "\033]12;")
	require.NotContains(t, string(output), "\033]112")

	// Set cursor color -> should emit OSC 12
	src.CursorColor = emu.RGBColor(255, 0, 0)
	output = Swap(dst, src)
	require.Contains(
		t,
		string(output),
		"\033]12;rgb:ff/00/00\033\\",
	)

	// Same color -> no change emitted
	dst.CursorColor = emu.RGBColor(255, 0, 0)
	output = Swap(dst, src)
	require.NotContains(t, string(output), "\033]12;")

	// Reset cursor color -> should emit OSC 112
	src.CursorColor = emu.DefaultCursor
	output = Swap(dst, src)
	require.Contains(t, string(output), "\033]112\033\\")
}
