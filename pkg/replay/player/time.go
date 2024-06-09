package player

import (
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
)

func (p *Player) Goto(index, offset int) {
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
				p.Terminal.Parse(data)
			}

			if i >= p.nextDetect {
				p.detector.Detect(p.Terminal, p.events)
				p.nextDetect = i + 1
			}
		case P.SizeMessage:
			p.Terminal.Resize(e.Vec())
		}
	}

	p.location.Index = toIndex
	p.location.Offset = toByte
	return
}
