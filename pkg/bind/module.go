package bind

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/bind/parse"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type Result[T any] struct {
	Scope  *Scope[T]
	Action T
}

type ScopeOptions struct {
	// The duration after which the client will return to the root scope if
	// no keys are pressed
	IdleTimeout time.Duration
	// If false, results lacking a destination scope will send client back
	// to the root scope, otherwise they will stay.
	StickyActions bool
}

var DEFAULT_SCOPE_OPTIONS = ScopeOptions{
	IdleTimeout:   200 * time.Millisecond,
	StickyActions: false,
}

type Scope[T any] struct {
	deadlock.RWMutex
	name     string
	mappings map[string]Result[T]
	options  ScopeOptions
}

func (s *Scope[T]) Lookup(key string) *Result[T] {
	s.RLock()
	defer s.RUnlock()

	if mapping, ok := s.mappings[key]; ok {
		return &mapping
	}

	return nil
}

type Event interface{}

// An action was triggered
type ActionEvent[T any] struct {
	Action T
}

// The scope changed
type ScopeEvent[T any] struct {
	Next *Scope[T]
}

// All input that did not match anything in the scope
type RawEvent struct {
	Data []byte
}

type Engine[T any] struct {
	util.Lifetime
	deadlock.RWMutex

	clients map[*Client[T]]struct{}
	root    *Scope[T]
}

func NewEngine[T any](ctx context.Context) *Engine[T] {
	options := DEFAULT_SCOPE_OPTIONS
	options.IdleTimeout = 0

	root := Scope[T]{
		name:     "*",
		mappings: make(map[string]Result[T]),
		options:  options,
	}

	return &Engine[T]{
		Lifetime: util.NewLifetime(ctx),
		root:     &root,
	}
}

type Client[T any] struct {
	util.Lifetime
	deadlock.RWMutex

	engine *Engine[T]
	scope  *Scope[T]
	events chan Event
}

func (c *Client[T]) Scope() *Scope[T] {
	c.RLock()
	defer c.RUnlock()

	return c.scope
}

func (c *Client[T]) setScope(scope *Scope[T]) {
	c.Lock()
	c.scope = scope
	c.Unlock()
	c.events <- ScopeEvent[T]{
		Next: scope,
	}
}

func (c *Client[T]) process(msg parse.Msg, data []byte) {
	scope := c.Scope()

	if key, ok := msg.(parse.KeyMsg); ok {
		result := scope.Lookup(key.String())
		if result == nil {
			// TODO go back to root?
			c.events <- RawEvent{
				Data: data,
			}
			return
		}

		if result.Scope != nil {
			c.setScope(result.Scope)
		} else {
			// go back to root
		}
	}
}

// Process input and produce events.
func (c *Client[T]) Input(data []byte) {
	for i, w := 0, 0; i < len(data); i += w {
		var msg parse.Msg
		w, msg = parse.DetectOneMsg(data[i:])
		c.process(msg, data[i:])
	}
}

func (c *Client[T]) Recv() <-chan Event {
	return c.events
}

func (e *Engine[T]) AddClient() *Client[T] {
	client := Client[T]{
		Lifetime: util.NewLifetime(e.Ctx()),
		events:   make(chan Event),
	}

	e.Lock()
	e.clients[&client] = struct{}{}
	e.Unlock()

	go func() {
		<-client.Ctx().Done()
		e.Lock()
		delete(e.clients, &client)
		e.Unlock()
	}()

	return &client
}
