package sessions

import (
	"time"

	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"

	"github.com/xo/terminfo"
)

// Simulator is a convenient way of building recorded sessions
// programmatically, primarily for testing.
type Simulator struct {
	events []Event
	info   *terminfo.Terminfo
}

func (s *Simulator) store(data P.Message) {
	s.events = append(s.events, Event{
		Stamp:   time.Now(),
		Message: data,
	})
}

func (s *Simulator) Write(data []byte) {
	s.store(P.OutputMessage{Data: data})
}

func (s *Simulator) Resize(size geom.Size) {
	s.store(P.SizeMessage{
		Columns: size.C,
		Rows:    size.R,
	})
}

// Term invokes terminfo's Printf method and adds it to the session.
func (s *Simulator) Term(i int, v ...interface{}) {
	s.Add(s.info.Printf(i, v...))
}

func (s *Simulator) Add(events ...interface{}) {
	for _, event := range events {
		switch event := event.(type) {
		case []byte:
			s.Write(event)
		case string:
			s.Write([]byte(event))
		case geom.Size:
			s.Resize(event)
		}
	}
}

func (s *Simulator) Events() []Event {
	return s.events
}

func NewSimulator() *Simulator {
	info, _ := terminfo.Load("xterm-256color")
	return &Simulator{
		info: info,
	}
}
