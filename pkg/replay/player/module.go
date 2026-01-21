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

	retainOutputData bool
	historyLimit     int

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
	options := make([]emu.TerminalOption, 0, 1)
	if p.historyLimit > 0 {
		options = append(options, emu.WithHistoryLimit(p.historyLimit))
	}

	p.Terminal = emu.New(options...)
}

func (p *Player) consume(event sessions.Event) {
	if p.retainOutputData {
		p.mu.Lock()
		p.events = append(p.events, event)
		index := len(p.events) - 1
		p.mu.Unlock()
		p.Goto(index, -1)
		return
	}

	p.mu.Lock()
	defer p.mu.Unlock()

	stored := event
	var outputData []byte
	switch e := event.Message.(type) {
	case P.OutputMessage:
		outputData = e.Data
		stored.Message = P.OutputMessage{Data: nil}
	}

	p.events = append(p.events, stored)
	index := len(p.events) - 1

	switch e := event.Message.(type) {
	case P.OutputMessage:
		if len(outputData) == 0 {
			break
		}

		// Need to clear dirty state before every write so that the detector works.
		p.Terminal.Changes().Reset()
		p.Parse(outputData)

		if index >= p.nextDetect {
			p.detector.Detect(p.Terminal, p.events)
			p.nextDetect = index + 1
		}
	case P.SizeMessage:
		p.Resize(e.Vec())
	}

	p.location.Index = index
	p.location.Offset = -1
}

func (p *Player) Release() {
	p.ReleaseProgress(nil)
}

// ReleaseProgress releases the player from replay mode and replays any events
// that were buffered while it was acquired. If update is non-nil, it will be
// called with (done,total) as buffered events are processed.
func (p *Player) ReleaseProgress(update func(done, total int)) {
	var (
		done        int
		total       int
		lastPercent = -1
	)

	p.mu.RLock()
	total = len(p.buffer)
	p.mu.RUnlock()

	maybeUpdate := func(force bool) {
		if update == nil || total <= 0 {
			return
		}

		percent := done * 100 / total
		if !force && percent == lastPercent {
			return
		}

		update(done, total)
		lastPercent = percent
	}

	maybeUpdate(true)

	for {
		p.mu.Lock()
		buffer := p.buffer
		p.buffer = nil
		p.mu.Unlock()

		if len(buffer) == 0 {
			p.mu.Lock()
			if len(p.buffer) == 0 {
				p.inUse = false
				p.mu.Unlock()
				break
			}
			p.mu.Unlock()
			continue
		}

		total = done + len(buffer)
		maybeUpdate(true)

		for _, event := range buffer {
			p.consume(event)
			done++
			maybeUpdate(false)
		}
	}

	if update != nil && done > 0 {
		total = done
		update(done, total)
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
	p := &Player{
		detector:         detect.New(options...),
		retainOutputData: true,
	}
	p.resetTerminal()
	return p
}

// SetRetainOutputData controls whether output bytes are kept in memory as part
// of the player's event log. Disabling this prevents seeking and Output()
// from working, but reduces memory usage when the recording is already
// persisted to disk.
func (p *Player) SetRetainOutputData(retain bool) {
	p.mu.Lock()
	p.retainOutputData = retain
	p.mu.Unlock()
}

// SetHistoryLimit bounds the amount of terminal scrollback kept in memory.
// This method is destructive and is intended to be called before any events
// are processed.
func (p *Player) SetHistoryLimit(limit int) {
	p.mu.Lock()
	defer p.mu.Unlock()

	p.historyLimit = limit
	p.events = nil
	p.buffer = nil
	p.location = search.Address{}
	p.nextDetect = 0
	p.resetTerminal()
}

func FromEvents(events []sessions.Event) *Player {
	player := New()

	for _, event := range events {
		_ = player.Process(event)
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

		_ = player.Process(event)
	}

	return player, nil
}
