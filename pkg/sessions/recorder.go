package sessions

import (
	"time"

	"github.com/cfoust/cy/pkg/mux/stream"

	"github.com/sasha-s/go-deadlock"
)

type Recorder struct {
	events []Event
	mutex  deadlock.RWMutex
	stream stream.Stream
}

var _ stream.Stream = (*Recorder)(nil)

func New() *Recorder {
	return &Recorder{}
}

func (s *Recorder) store(data EventData) {
	s.mutex.Lock()
	defer s.mutex.Unlock()
	s.events = append(s.events, Event{
		Stamp: time.Now(),
		Data:  data,
	})
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
	s.store(OutputEvent{Bytes: data})
	return
}

func (s *Recorder) Resize(size stream.Size) error {
	s.store(ResizeEvent{
		Columns: size.C,
		Rows:    size.R,
	})

	return s.stream.Resize(size)
}

func NewRecorder(stream stream.Stream) *Recorder {
	return &Recorder{
		events: make([]Event, 0),
		stream: stream,
	}
}
