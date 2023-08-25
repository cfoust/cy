package screen

import (
	"context"
	"io"

	"github.com/cfoust/cy/pkg/geom"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/xo/terminfo"
)

type teaScreen struct {
	reads   *io.PipeReader
	writes  *io.PipeWriter
	program *tea.Program
}

var _ Stream = (*teaScreen)(nil)

func (s *teaScreen) Resize(size Size) error {
	s.program.Send(tea.WindowSizeMsg{
		Width:  size.C,
		Height: size.R,
	})
	return nil
}

func (s *teaScreen) Write(data []byte) (n int, err error) {
	return s.writes.Write(data)
}

func (s *teaScreen) Read(p []byte) (n int, err error) {
	return s.reads.Read(p)
}

func newTeaScreen(ctx context.Context, model tea.Model, size Size) *teaScreen {
	reads, out := io.Pipe()
	in, writes := io.Pipe()

	program := tea.NewProgram(
		model,
		tea.WithContext(ctx),
		tea.WithAltScreen(),
		tea.WithOutput(out),
		tea.WithInput(in),
	)

	go program.Run()

	tea := &teaScreen{
		reads:   reads,
		writes:  writes,
		program: program,
	}

	return tea
}

type Tea struct {
	*Terminal
	stream *teaScreen
}

var _ Screen = (*Tea)(nil)

func NewTea(
	ctx context.Context,
	model tea.Model,
	info RenderContext,
	size Size,
) *Tea {
	stream := newTeaScreen(
		ctx,
		model,
		geom.DEFAULT_SIZE,
	)

	terminal := NewTerminal(ctx, stream, geom.DEFAULT_SIZE)
	// bubbletea uses a simulated cursor
	info.Fprintf(terminal, terminfo.CursorInvisible)

	return &Tea{
		Terminal: terminal,
		stream:   stream,
	}
}
