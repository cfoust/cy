package bind

import (
	"context"

	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type Result interface{}

type Scope struct {
	name     string
	mappings map[string]Result
}

type Event interface{}

// An action was triggered
type ActionEvent[T any] struct {
	Action T
}

// The scope changed
type ScopeEvent struct {
	Next *Scope
}

// All input that did not match anything in the scope
type RawEvent struct {
	Data []byte
}

type Engine[T any] struct {
	util.Lifetime
	deadlock.RWMutex

	clients map[*Client[T]]struct{}
}

func NewEngine[T any](ctx context.Context) *Engine[T] {
	return &Engine[T]{
		Lifetime: util.NewLifetime(ctx),
	}
}

type Client[T any] struct {
	util.Lifetime
	deadlock.RWMutex

	engine *Engine[T]
	scope  *Scope
	// The shortest path to the user's current scope
	path []*Scope

	events chan Event
}

func (c *Client[T]) Input(data []byte) {
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
