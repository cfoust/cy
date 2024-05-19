package player

import (
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/replay/movement"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/sessions/search"

	"github.com/sasha-s/go-deadlock"
)

const (
	CY_HOOK = "cy"
)

type Player struct {
	emu.Terminal
	mu deadlock.RWMutex

	inUse bool

	location   search.Address
	buffer     []sessions.Event
	events     []sessions.Event
	nextDetect int

	commands []Command
	// If we have ever detected a prompt
	havePrompt bool

	from   geom.Vec2
	fromID emu.WriteID
}

var _ sessions.EventHandler = (*Player)(nil)

func (p *Player) Acquire() {
	p.mu.Lock()
	p.inUse = true
	p.mu.Unlock()
}

func (p *Player) resetTerminal() {
	p.Terminal = emu.New()
	p.Terminal.Changes().SetHooks([]string{CY_HOOK})
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

// Preview captures a preview with the size `viewport` at `location` in the
// scrollback of the terminal. You may also provide `highlights` that will be
// passed to the Flow renderer. Returns nil if the player is "in use", which is
// only used by replay mode to jump back in time.
func (p *Player) Preview(
	viewport, location geom.Vec2,
	highlights []movement.Highlight,
) *tty.State {
	if p.getInUse() {
		return nil
	}

	p.mu.Lock()
	defer p.mu.Unlock()
	return movement.PreviewFlow(p, viewport, location, highlights)
}

func New() *Player {
	p := &Player{}
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
