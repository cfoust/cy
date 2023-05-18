package proxy

import (
	"time"
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
