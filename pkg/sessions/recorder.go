package sessions

import (
	"time"

	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/mux/stream"

	"github.com/sasha-s/go-deadlock"
)

type Recorder struct {
	writer SessionWriter

	events []Event
	mutex  deadlock.RWMutex
	stream stream.Stream
}

var _ stream.Stream = (*Recorder)(nil)

func (s *Recorder) store(data P.Message) error {
	s.mutex.Lock()
	defer s.mutex.Unlock()

	event := Event{
		Stamp:   time.Now(),
		Message: data,
	}

	s.events = append(s.events, event)

	return nil
}

func (s *Recorder) Events() []Event {
	s.mutex.Lock()
	events := s.events
	s.mutex.Unlock()
	return events
}

func (s *Recorder) Write(data []byte) (n int, err error) {
	return s.stream.Write(data)
}

func (s *Recorder) Read(p []byte) (n int, err error) {
	n, err = s.stream.Read(p)
	if err != nil {
		return n, err
	}

	data := make([]byte, n)
	copy(data, p)
	s.store(P.OutputMessage{Data: data})
	return
}

func (s *Recorder) Resize(size stream.Size) error {
	s.store(P.SizeMessage{
		Columns: size.C,
		Rows:    size.R,
	})

	return s.stream.Resize(size)
}

func NewRecorder(filename string, stream stream.Stream) (*Recorder, error) {
	r := &Recorder{
		events: make([]Event, 0),
		stream: stream,
	}

	return r, nil
}
