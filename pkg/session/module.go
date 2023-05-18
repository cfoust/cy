package session

import (
	"time"

	"github.com/cfoust/cy/pkg/util"
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
	Width  int
	Height int
}

func (i ResizeEvent) Type() EventType { return EventTypeResize }

type Event struct {
	Stamp time.Time
	Data  EventData
}

type Session struct {
	events []Event
	mutex  deadlock.RWMutex
}

func New() *Session {
	return &Session{
		events: make([]Event, 0),
	}
}

func (s *Session) store(data EventData) {
	s.mutex.Lock()
	defer s.mutex.Unlock()
	s.events = append(s.events, Event{
		Stamp: time.Now(),
		Data:  data,
	})
}

func (s *Session) Input(data []byte) {
	s.store(InputEvent{Bytes: data})
}

func (s *Session) Output(data []byte) {
	s.store(OutputEvent{Bytes: data})
}

func (s *Session) Resize(width, height int) {
	s.store(ResizeEvent{
		Width:  width,
		Height: height,
	})
}

var _ util.Resizable = (*Session)(nil)
