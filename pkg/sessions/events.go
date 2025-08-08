package sessions

import (
	"context"
	"time"

	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/mux/stream"

	"github.com/sasha-s/go-deadlock"
)

type Event struct {
	Stamp   time.Time
	Message P.Message
}

// An EventHandler handles a stream of terminal events.
type EventHandler interface {
	Process(event Event) error
}

// An EventStream is a proxy for a Stream that turns everything that happens on
// that Stream into Event structs and passes them to the provided handler.
type EventStream struct {
	stream  stream.Stream
	handler EventHandler
}

var _ stream.Stream = (*EventStream)(nil)

func (s *EventStream) Kill() {
	s.stream.Kill()
}

func (s *EventStream) process(data P.Message) error {
	event := Event{
		Stamp:   time.Now(),
		Message: data,
	}
	return s.handler.Process(event)
}

func (s *EventStream) Write(data []byte) (n int, err error) {
	return s.stream.Write(data)
}

func (s *EventStream) Read(p []byte) (n int, err error) {
	n, err = s.stream.Read(p)

	if err != nil {
		return n, err
	}

	data := make([]byte, n)
	copy(data, p)
	err = s.process(P.OutputMessage{Data: data})
	return
}

func (s *EventStream) Resize(size stream.Size) error {
	err := s.stream.Resize(size)
	if err != nil {
		return err
	}

	err = s.process(P.SizeMessage{
		Columns: size.C,
		Rows:    size.R,
	})
	if err != nil {
		return err
	}

	return nil
}

func NewEventStream(stream stream.Stream, handler EventHandler) *EventStream {
	return &EventStream{
		stream:  stream,
		handler: handler,
	}
}

// A MemoryRecorder stores Events in memory.
type MemoryRecorder struct {
	mutex  deadlock.RWMutex
	events []Event
}

var _ EventHandler = (*MemoryRecorder)(nil)

func (r *MemoryRecorder) Events() []Event {
	r.mutex.Lock()
	events := r.events
	r.mutex.Unlock()
	return events
}

func (r *MemoryRecorder) Process(event Event) error {
	r.mutex.Lock()
	defer r.mutex.Unlock()
	r.events = append(r.events, event)
	return nil
}

func NewMemoryRecorder() *MemoryRecorder {
	return &MemoryRecorder{}
}

// A FileRecorder writes incoming events to a file.
type FileRecorder struct {
	eventc chan Event
	flushc chan struct{}
}

var _ EventHandler = (*FileRecorder)(nil)

func (f *FileRecorder) Process(event Event) error {
	f.eventc <- event
	return nil
}

func (f *FileRecorder) Flush() error {
	f.flushc <- struct{}{}
	return nil
}

func NewFileRecorder(
	ctx context.Context,
	filename string,
) (*FileRecorder, error) {
	f := &FileRecorder{
		eventc: make(chan Event, 100),
		flushc: make(chan struct{}),
	}

	w, err := Create(filename)
	if err != nil {
		return nil, err
	}

	go func() {
		defer func() { _ = w.Close() }()
		for {
			// TODO(cfoust): 09/19/23 error handling
			select {
			case event := <-f.eventc:
				_ = w.Write(event)
			case <-f.flushc:
				_ = w.Flush()
			case <-ctx.Done():
				return
			}
		}
	}()

	return f, nil
}

// A MultiplexHandler chains multiple EventHandlers together in sequence, but
// returns a single EventHandler.
type MultiplexHandler struct {
	handlers []EventHandler
}

var _ EventHandler = (*MultiplexHandler)(nil)

func (m *MultiplexHandler) Process(event Event) error {
	for _, handler := range m.handlers {
		err := handler.Process(event)
		if err != nil {
			return err
		}
	}

	return nil
}

func NewMultiplexHandler(handlers ...EventHandler) *MultiplexHandler {
	return &MultiplexHandler{
		handlers: handlers,
	}
}
