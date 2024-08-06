package tty

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"

	"github.com/charmbracelet/lipgloss"
	"github.com/muesli/termenv"
	"github.com/stretchr/testify/require"
	"github.com/xo/terminfo"
)

func testBytes(
	t *testing.T,
	name string,
	bytes []byte,
) {
	info, _ := terminfo.Load("xterm-256color")
	termA := emu.New()
	termA.Write(bytes)

	termB := emu.New()
	newBytes := swapImage(
		info,
		termB.Screen(),
		termA.Screen(),
	)
	termB.Write(newBytes)

	require.Equal(
		t,
		termA.Screen(),
		termB.Screen(),
		"style %s was not equal: %#v",
		name,
		string(newBytes),
	)
	t.Logf("%s %+v", name, termB.Screen()[0][0])
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
	} {
		testBytes(t, name, []byte(style.Render("foobar")))
	}

	testBytes(t, "style", []byte("\033[48;2;255;0;0m           \033[0m\033[3;38;2;0;0;255;48;2;255;0;0mtest\033[0m"))
}
