package stream

import (
	"time"

	"github.com/sasha-s/go-deadlock"
)

type EventType byte

const (
	EventTypeInput EventType = iota
	EventTypeOutput
	EventTypeResize
)

type EventData interface {
	Type() EventType
}

type Write struct {
	Bytes []byte
}

type InputEvent Write

func (i InputEvent) Type() EventType { return EventTypeInput }

type OutputEvent Write

func (i OutputEvent) Type() EventType { return EventTypeOutput }

type ResizeEvent struct {
	Columns int
	Rows    int
}

func (i ResizeEvent) Type() EventType { return EventTypeResize }

type Event struct {
	Stamp time.Time
	Data  EventData
}

type Recorder struct {
	events []Event
	mutex  deadlock.RWMutex
	stream Stream
}

var _ Stream = (*Recorder)(nil)

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
	s.store(InputEvent{Bytes: data})
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

func (s *Recorder) Resize(size Size) error {
	s.store(ResizeEvent{
		Columns: size.C,
		Rows:    size.R,
	})

	return s.stream.Resize(size)
}

func NewRecorder(stream Stream) *Recorder {
	return &Recorder{
		events: make([]Event, 0),
		stream: stream,
	}
}
