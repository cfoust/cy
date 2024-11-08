package player

import (
	"context"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/replay/detect"
	"github.com/cfoust/cy/pkg/replay/movement"
	"github.com/cfoust/cy/pkg/replay/movement/flow"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/sessions/search"

	"github.com/sasha-s/go-deadlock"
)

type Player struct {
	emu.Terminal

	detector *detect.Detector
	mu       deadlock.RWMutex

	inUse bool

	location   search.Address
	buffer     []sessions.Event
	events     []sessions.Event
	nextDetect int
}

var _ sessions.EventHandler = (*Player)(nil)

func (p *Player) Acquire() {
	p.mu.Lock()
	p.inUse = true
	p.mu.Unlock()
}

func (p *Player) resetTerminal() {
	p.Terminal = emu.New()
	p.Terminal.Changes().SetHooks([]string{detect.CY_HOOK})
}

func (p *Player) consume(event sessions.Event) {
	p.mu.Lock()
	p.events = append(p.events, event)
	p.mu.Unlock()
	p.Goto(len(p.events)-1, -1)
}

func (p *Player) Release() {
	p.mu.Lock()
	p.inUse = false
	buffer := p.buffer
	p.buffer = make([]sessions.Event, 0)
	p.mu.Unlock()

	for _, event := range buffer {
		p.consume(event)
	}
}

// Output gets all of the output written in the range [start, end).
func (p *Player) Output(start, end int) (data []byte, ok bool) {
	p.mu.RLock()
	defer p.mu.RUnlock()

	events := p.events

	if start < 0 || start >= len(events) {
		return
	}

	if end < 0 || end > len(events) {
		return
	}

	ok = true

	if start >= end {
		return
	}

	for i := start; i < end; i++ {
		event := events[i]
		switch e := event.Message.(type) {
		case P.OutputMessage:
			data = append(data, e.Data...)
		}
	}

	return
}

func (p *Player) Events() []sessions.Event {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return p.events
}

func (p *Player) Location() search.Address {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return p.location
}

func (p *Player) getInUse() bool {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return p.inUse
}

func (p *Player) Process(event sessions.Event) error {
	if !p.getInUse() {
		p.consume(event)
		return nil
	}

	p.mu.Lock()
	p.buffer = append(p.buffer, event)
	p.mu.Unlock()
	return nil
}

func (p *Player) Commands() []detect.Command {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return p.detector.Commands(p.Terminal, p.events)
}

// Preview captures a preview with the size `viewport` at `location` in the
// scrollback of the terminal. You may also provide `highlights` that will be
// passed to the Flow renderer. Returns nil if the player is "in use", which is
// only used by replay mode to jump back in time.
func (p *Player) Preview(
	params *params.Parameters,
	viewport, location geom.Vec2,
	highlights []movement.Highlight,
) *tty.State {
	// Get the last line referenced in this preview request
	lastLine := location.R
	for _, highlight := range highlights {
		lastLine = geom.Max(highlight.From.R, lastLine)
		lastLine = geom.Max(highlight.To.R, lastLine)
	}

	p.mu.RLock()
	numLines := p.Flow(p.Size(), p.Root()).NumLines
	p.mu.RUnlock()

	// If the player is back in time, we can't preview lines we don't have
	// yet. This is not perfect, because lines that are on the screen can
	// still change, but it's easier than forcing the user to provide an
	// event index. Might want to revisit this someday.
	if lastLine >= numLines {
		return nil
	}

	p.mu.Lock()
	defer p.mu.Unlock()
	return flow.PreviewFlow(
		params,
		p,
		viewport,
		location,
		highlights,
	)
}

func New(options ...detect.Option) *Player {
	p := &Player{detector: detect.New(options...)}
	p.resetTerminal()
	return p
}

func FromEvents(events []sessions.Event) *Player {
	player := New()

	for _, event := range events {
		player.Process(event)
	}

	return player
}

// FromEventsContext creates a Player from the given events, but stops if the
// context is cancelled.
func FromEventsContext(
	ctx context.Context,
	events []sessions.Event,
) (*Player, error) {
	player := New()

	for _, event := range events {
		if ctx.Err() != nil {
			return nil, ctx.Err()
		}

		player.Process(event)
	}

	return player, nil
}
