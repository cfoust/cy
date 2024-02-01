package replay

import (
	"context"
	"fmt"
	"io"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type ReplayPreview struct {
	util.Lifetime
	render   *taro.Renderer
	replay   *taro.Program
	err      error
	filename string
	size     geom.Vec2
}

var _ taro.Model = (*ReplayPreview)(nil)

type loadedEvent struct {
	replay *taro.Program
	err    error
}

func (r *ReplayPreview) Init() tea.Cmd {
	size := r.size
	return func() tea.Msg {
		reader, err := sessions.Open(r.filename)
		if err != nil {
			return loadedEvent{
				err: err,
			}
		}

		events := make([]sessions.Event, 0)
		for {
			event, err := reader.Read()
			if err == io.EOF || err == io.ErrUnexpectedEOF {
				break
			}
			if err != nil {
				return loadedEvent{
					err: err,
				}
			}
			events = append(events, event)
		}

		ctx := r.Lifetime.Ctx()
		replay := New(
			ctx,
			events,
			bind.NewBindScope(nil),
		)
		replay.Resize(size)

		return loadedEvent{
			replay: replay,
		}
	}
}

func (r *ReplayPreview) View(state *tty.State) {
	if r.err != nil {
		r.render.RenderAt(
			state.Image,
			0, 0,
			lipgloss.Place(
				geom.DEFAULT_SIZE.C,
				geom.DEFAULT_SIZE.R,
				lipgloss.Center, lipgloss.Center,
				fmt.Sprintf("error: %s", r.err.Error()),
			),
		)
		return
	}

	if r.replay == nil {
		r.render.RenderAt(
			state.Image,
			0, 0,
			lipgloss.Place(
				geom.DEFAULT_SIZE.C,
				geom.DEFAULT_SIZE.R,
				lipgloss.Center, lipgloss.Center,
				fmt.Sprintf("loading %s...", r.filename),
			),
		)
		return
	}

	tty.Copy(geom.Vec2{}, state, r.replay.State())
}

func (r *ReplayPreview) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		r.size = geom.Size{
			R: msg.Height,
			C: msg.Width,
		}

		if r.replay != nil {
			r.replay.Resize(r.size)
		}
	case loadedEvent:
		if msg.err != nil {
			r.err = msg.err
			return r, nil
		}

		r.replay = msg.replay
		r.replay.Resize(r.size)
	}

	return r, nil
}

func NewPreview(
	ctx context.Context,
	filename string,
) *taro.Program {
	program := taro.New(ctx, &ReplayPreview{
		Lifetime: util.NewLifetime(ctx),
		render:   taro.NewRenderer(),
		filename: filename,
	})
	return program
}
