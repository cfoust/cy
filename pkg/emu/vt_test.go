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

func TestPrompt(t *testing.T) {
	term := New()
	dirty := term.Changes()
	dirty.SetHooks([]string{"cy"})

	_, err := term.Write([]byte("\033Pcy\033\\"))
	require.NoError(t, err)

	value, ok := dirty.Hook("cy")
	require.True(t, value)
	require.True(t, ok)
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
