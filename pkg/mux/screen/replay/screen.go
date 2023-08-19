package replay

import (
	"context"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux/screen"
	"github.com/cfoust/cy/pkg/mux/stream"
	"github.com/cfoust/cy/pkg/util"

	"github.com/charmbracelet/lipgloss"
	"github.com/muesli/termenv"
	"github.com/sasha-s/go-deadlock"
)

type Replay struct {
	deadlock.RWMutex

	util.Lifetime
	*screen.Trigger

	events   []stream.Event
	terminal emu.Terminal
	recorder *stream.Recorder
	overlay  *screen.Tea
	model    *model

	index int
}

var _ screen.Screen = (*Replay)(nil)

// Move the terminal from event index `from` to `to`.
func (c *Replay) update(from, to int) {
	from = geom.Clamp(from, 0, len(c.events)-1)
	to = geom.Clamp(to, 0, len(c.events)-1)

	if from == to {
		return
	}

	origin := from + 1
	if from > to {
		c.terminal = emu.New()
		origin = 0
	}

	for i := origin; i <= to; i++ {
		event := c.events[i]
		switch e := event.Data.(type) {
		case stream.OutputEvent:
			c.terminal.Write(e.Bytes)
		case stream.ResizeEvent:
			c.terminal.Resize(
				e.Columns,
				e.Rows,
			)
		}
	}

	c.index = to
}

func (c *Replay) Render(size screen.Size) *tty.State {
	c.Lock()
	defer c.Unlock()
	from := c.index
	to := c.model.index
	if from != to {
		c.update(from, to)
	}

	state := tty.Capture(c.terminal)
	stateSize := state.Image.Size()

	out := tty.New(size)
	out.CursorVisible = false

	// TODO(cfoust): 08/10/23 indicate smaller/bigger size somehow
	for row := 0; row < size.R && row < stateSize.R; row++ {
		for col := 0; col < size.C && col < stateSize.C; col++ {
			out.Image[row][col] = state.Image[row][col]
		}
	}

	image.Compose(geom.Vec2{}, out.Image, c.overlay.State().Image)

	out.Cursor = state.Cursor

	return out
}

func (c *Replay) Write(data []byte) (n int, err error) {
	n, err = c.overlay.Write(data)
	c.Rerender()
	return
}


func (c *Replay) poll(ctx context.Context) {
	updates := c.overlay.Updates()
	defer updates.Done()
	for {
		select {
		case <-ctx.Done():
			return
		case <-updates.Recv():
			c.Rerender()
		}
	}
}

func (c *Replay) Resize(size screen.Size) error {
	err := c.overlay.Resize(size)
	if err != nil {
		return err
	}

	return c.Trigger.Resize(size)
}

func New(
	ctx context.Context,
	info screen.RenderContext,
	recorder *stream.Recorder,
	size screen.Size,
) *Replay {
	lifetime := util.NewLifetime(ctx)

	events := recorder.Events()
	m := &model{
		lifetime:  &lifetime,
		events:    events,
		renderer: lipgloss.NewRenderer(
			nil,
			termenv.WithProfile(info.Colors),
		),
	}
	m.setIndex(-1)

	overlay := screen.NewTea(
		lifetime.Ctx(),
		m,
		info,
		size,
	)

	c := &Replay{
		Lifetime: lifetime,
		terminal: emu.New(),
		overlay:  overlay,
		recorder: recorder,
		model:    m,
		events:   events,
	}
	c.Trigger = screen.NewTrigger(c)

	go c.poll(lifetime.Ctx())

	return c
}
