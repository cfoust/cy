package player

import (
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
)

// GotoProgress is the same as Goto, but it allows for a progress channel to be
// passed in. The channel will receive progress updates as the player moves
// through the events.
func (p *Player) GotoProgress(
	index, offset int,
	progress chan<- int,
) {
	p.mu.Lock()
	defer p.mu.Unlock()

	numEvents := len(p.events)
	if numEvents == 0 {
		return
	}

	// Allow for negative indices from end of stream
	if index < 0 {
		index = geom.Clamp(numEvents+index, 0, numEvents-1)
	}

	fromIndex := geom.Clamp(p.location.Index, 0, numEvents-1)
	toIndex := geom.Clamp(index, 0, numEvents-1)
	fromByte := p.location.Offset
	toByte := offset

	// Going back in time; must start over
	if toIndex < fromIndex || (toIndex == fromIndex && toByte < fromByte) {
		p.resetTerminal()
		fromIndex = 0
		fromByte = -1
	}

	var oldProgress, newProgress int
	distance := toIndex - fromIndex
	for i := fromIndex; i <= toIndex; i++ {
		event := p.events[i]
		switch e := event.Message.(type) {
		case P.OutputMessage:
			data := e.Data

			if toIndex == i {
				if toByte < 0 {
					toByte += len(data)
				}
				toByte = geom.Clamp(toByte, 0, len(data)-1)
			}

			if len(data) > 0 {
				if fromIndex == toIndex {
					data = data[fromByte+1 : toByte+1]
				} else if fromIndex == i {
					data = data[fromByte+1:]
				} else if toIndex == i {
					data = data[:toByte+1]
				}
			}

			if len(data) > 0 {
				// Need to clear dirty state before every write
				// so that the detector works
				p.Terminal.Changes().Reset()
				p.Terminal.Parse(data)
			}

			if i >= p.nextDetect {
				p.detector.Detect(p.Terminal, p.events)
				p.nextDetect = i + 1
			}
		case P.SizeMessage:
			p.Terminal.Resize(e.Vec())
		}

		if progress == nil {
			continue
		}

		newProgress = int(float64(i-fromIndex) / float64(distance) * 100)
		if newProgress == oldProgress {
			continue
		}
		progress <- newProgress
		oldProgress = newProgress
	}

	if progress != nil {
		close(progress)
	}

	p.location.Index = toIndex
	p.location.Offset = toByte
	return
}

// Goto moves the player to the specified event index and byte offset. If the
// index and offset are before the current location, the player will reset the
// terminal and start from the beginning, since there's no way to "rewind" a
// terminal session. This means that playing backwards is very slow on long
// sessions.
func (p *Player) Goto(index, offset int) {
	p.GotoProgress(index, offset, nil)
}
