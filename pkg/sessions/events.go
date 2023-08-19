package sessions

import (
	"time"
)

type EventType byte

const (
	EventTypeOutput EventType = iota
	EventTypeResize
)

type EventData interface {
	Type() EventType
}

type Write struct {
	Bytes []byte
}

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
