package emu

import (
	"io"
	"strings"
	"testing"

	"github.com/cfoust/cy/pkg/geom"

	"github.com/stretchr/testify/require"
)

func extractStr(term Terminal, x0, x1, row int) string {
	var s []rune
	for i := x0; i <= x1; i++ {
		attr := term.Cell(i, row)
		s = append(s, attr.Char)
	}
	return string(s)
}

func TestPlainChars(t *testing.T) {
	term := New()
	expected := "Hello world!"
	_, err := term.Write([]byte(expected))
	if err != nil && err != io.EOF {
		t.Fatal(err)
	}
	actual := extractStr(term, 0, len(expected)-1, 0)
	if expected != actual {
		t.Fatal(actual)
	}
}

func TestNewline(t *testing.T) {
	term := New()
	expected := "Hello world!\n...and more."
	_, err := term.Write([]byte(LineFeedMode))
	if err != nil && err != io.EOF {
		t.Fatal(err)
	}
	_, err = term.Write([]byte(expected))
	if err != nil && err != io.EOF {
		t.Fatal(err)
	}

	split := strings.Split(expected, "\n")
	actual := extractStr(term, 0, len(split[0])-1, 0)
	actual += "\n"
	actual += extractStr(term, 0, len(split[1])-1, 1)
	if expected != actual {
		t.Fatal(actual)
	}

	// A newline with a color set should not make the next line that color,
	// which used to happen if it caused a scroll event.
	st := (term.(*terminal))
	st.moveTo(0, st.rows-1)
	_, err = term.Write([]byte("\033[1;37m\n$ \033[m"))
	if err != nil && err != io.EOF {
		t.Fatal(err)
	}
	cur := term.Cursor()
	attr := term.Cell(cur.C, cur.R)
	if attr.FG != DefaultFG {
		t.Fatal(st.cur.C, st.cur.R, attr.FG, attr.BG)
	}
}

func TestRoot(t *testing.T) {
	term := New()
	term.Resize(geom.Vec2{C: 6, R: 2})
	_, _ = term.Write([]byte(LineFeedMode))
	_, _ = term.Write([]byte("foo\nbar"))
	require.Equal(t, geom.Vec2{}, term.Root())
	_, _ = term.Write([]byte("\nbaz"))
	require.Equal(t, geom.Vec2{
		R: 1,
		C: 0,
	}, term.Root())

	// Wrap onto screen
	_, _ = term.Write([]byte("foobar\n"))
	require.Equal(t, geom.Vec2{
		R: 2,
		C: 6,
	}, term.Root())

	// Root() should return result from main screen, alt screen does not
	// have root
	_, _ = term.Write([]byte(EnterAltScreen))
	_, _ = term.Write([]byte("test\ntest\ntest"))
	require.Equal(t, geom.Vec2{
		R: 2,
		C: 6,
	}, term.Root())
}

func TestTabsBug(t *testing.T) {
	term := New()
	// This is the simplest example of a bug that I encountered with tabs.
	term.Resize(geom.Vec2{C: 172, R: 3})
	_, _ = term.Write([]byte(LineFeedMode))
	_, _ = term.Write(
		[]byte(
			"LICENSE\t\tcmd\t\tdaemon.log\terr.log\t\tgo.sum\t\tmain\t\tscreenshot.gif\tstories.log\ttrace.prof\r\n",
		),
	)
	first := term.Screen()[0].String()
	index := strings.Index(first, "trace.prof")
	require.NotEqual(t, -1, index)
}

func TestResizeBug(t *testing.T) {
	term := New(WithoutHistory)
	term.Resize(geom.Vec2{C: 6, R: 2})
	_, _ = term.Write([]byte("foobarbaz"))
	term.Resize(geom.Vec2{C: 6, R: 1})
	term.Resize(geom.Vec2{C: 6, R: 2})
}

func TestSGR(t *testing.T) {
	type check struct {
		col  int
		mode int16
		set  bool
		msg  string
	}
	type fgCheck struct {
		col int
		msg string
	}

	tests := []struct {
		name     string
		input    string
		checks   []check
		fgChecks []fgCheck
	}{
		{
			name:  "many params",
			input: "\033[0;4;4;7;1;3;9;38;2;255;200;0;48;2;0;0;200m text\033[m",
			fgChecks: []fgCheck{
				{1, "17-param SGR should apply foreground color"},
			},
		},
		{
			name:  "dim",
			input: "\033[2mfaded\033[m",
			checks: []check{
				{0, AttrDim, true, "SGR 2 should set dim"},
				{6, AttrDim, false, "SGR 0 should clear dim"},
			},
		},
		{
			name:  "hidden",
			input: "\033[8mhidden\033[m",
			checks: []check{
				{0, AttrHidden, true, "SGR 8 should set hidden"},
			},
		},
		{
			name:  "overline",
			input: "\033[53mover\033[m",
			checks: []check{
				{0, AttrOverline, true, "SGR 53 should set overline"},
			},
		},
		{
			name:  "22 clears bold and dim",
			input: "\033[1;2mboth\033[22mneither\033[m",
			checks: []check{
				{0, AttrBold, true, "bold should be set"},
				{0, AttrDim, true, "dim should be set"},
				{4, AttrBold, false, "SGR 22 should clear bold"},
				{4, AttrDim, false, "SGR 22 should clear dim"},
			},
		},
		{
			name:  "28 clears hidden",
			input: "\033[8mhid\033[28mvis\033[m",
			checks: []check{
				{0, AttrHidden, true, "hidden should be set"},
				{3, AttrHidden, false, "SGR 28 should clear hidden"},
			},
		},
		{
			name:  "55 clears overline",
			input: "\033[53mover\033[55mno\033[m",
			checks: []check{
				{0, AttrOverline, true, "overline should be set"},
				{4, AttrOverline, false, "SGR 55 should clear overline"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			term := New()
			_, err := term.Write([]byte(tt.input))
			require.NoError(t, err)

			for _, c := range tt.checks {
				attr := term.Cell(c.col, 0)
				if c.set {
					require.NotEqual(
						t,
						int16(0),
						attr.Mode&c.mode,
						c.msg,
					)
				} else {
					require.Equal(
						t,
						int16(0),
						attr.Mode&c.mode,
						c.msg,
					)
				}
			}

			for _, c := range tt.fgChecks {
				attr := term.Cell(c.col, 0)
				require.NotEqual(
					t,
					DefaultFG,
					attr.FG,
					c.msg,
				)
			}
		})
	}
}

func TestBracketedPasteMode(t *testing.T) {
	term := New()

	// Initially bracketed paste should be off
	require.Equal(t, ModeFlag(0), term.Mode()&ModeBracketedPaste)

	// Enable bracketed paste mode with CSI ?2004h
	_, err := term.Write([]byte("\x1b[?2004h"))
	require.NoError(t, err)
	require.Equal(t, ModeBracketedPaste, term.Mode()&ModeBracketedPaste)

	// Disable bracketed paste mode with CSI ?2004l
	_, err = term.Write([]byte("\x1b[?2004l"))
	require.NoError(t, err)
	require.Equal(t, ModeFlag(0), term.Mode()&ModeBracketedPaste)
}
