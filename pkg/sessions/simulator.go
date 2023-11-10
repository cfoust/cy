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

func (s *Simulator) store(delta time.Duration, data P.Message) {
	event := Event{
		Message: data,
	}

	if len(s.events) != 0 {
		event.Stamp = s.events[len(s.events)-1].Stamp.Add(delta)
	}

	s.events = append(s.events, event)
}

func (s *Simulator) WriteTime(delta time.Duration, data []byte) *Simulator {
	s.store(delta, P.OutputMessage{Data: data})
	return s
}

func (s *Simulator) Write(data []byte) *Simulator {
	s.store(0, P.OutputMessage{Data: data})
	return s
}

func (s *Simulator) ResizeTime(delta time.Duration, size geom.Size) *Simulator {
	s.store(delta, P.SizeMessage{
		Columns: size.C,
		Rows:    size.R,
	})
	return s
}

func (s *Simulator) Resize(size geom.Size) *Simulator {
	s.store(0, P.SizeMessage{
		Columns: size.C,
		Rows:    size.R,
	})
	return s
}

func (s *Simulator) AddTime(delta time.Duration, event interface{}) *Simulator {
	switch event := event.(type) {
	case []byte:
		s.WriteTime(delta, event)
	case string:
		s.WriteTime(delta, []byte(event))
	case geom.Size:
		s.ResizeTime(delta, event)
	}
	return s
}

func (s *Simulator) TermTime(delta time.Duration, i int, v ...interface{}) *Simulator {
	s.AddTime(delta, s.info.Printf(i, v...))
	return s
}

// Term invokes terminfo's Printf method and adds it to the session.
func (s *Simulator) Term(i int, v ...interface{}) *Simulator {
	s.AddTime(0, s.info.Printf(i, v...))
	return s
}

func (s *Simulator) Add(events ...interface{}) *Simulator {
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
	return s
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
