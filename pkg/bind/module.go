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
	// no keys are pressed. Zero means never.
	IdleTimeout time.Duration
	// If true, triggering an action does not return the client to the root scope.
	StickyActions bool
	// If true, unmatched key presses will not return the client to the root scope.
	StickyWrites bool
}

var DEFAULT_SCOPE_OPTIONS = ScopeOptions{
	IdleTimeout:   200 * time.Millisecond,
	StickyWrites:  false,
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

func (s *Scope[T]) Bind(key string, action Result[T]) {
	s.Lock()
	defer s.Unlock()

	s.mappings[key] = action
}

func (s *Scope[T]) Options() ScopeOptions {
	s.RLock()
	defer s.RUnlock()

	return s.options
}

func NewScope[T any]() *Scope[T] {
	return &Scope[T]{
		mappings: make(map[string]Result[T]),
		options:  DEFAULT_SCOPE_OPTIONS,
	}
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

func (e *Engine[T]) Root() *Scope[T] {
	e.RLock()
	defer e.RUnlock()
	return e.root
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
		clients:  make(map[*Client[T]]struct{}),
	}
}

type Client[T any] struct {
	util.Lifetime
	deadlock.RWMutex

	engine        *Engine[T]
	scope         *Scope[T]
	events        chan Event
	scopeLifetime util.Lifetime
}

func (c *Client[T]) Scope() *Scope[T] {
	c.RLock()
	defer c.RUnlock()
	return c.scope
}

func (c *Client[T]) setScope(scope *Scope[T]) {
	c.scopeLifetime.Cancel()

	c.Lock()
	c.scope = scope
	c.scopeLifetime = util.NewLifetime(c.Ctx())
	timeout := scope.options.IdleTimeout
	c.Unlock()

	c.events <- ScopeEvent[T]{
		Next: scope,
	}

	if timeout == 0 {
		return
	}

	go func() {
		timer := time.NewTimer(timeout)
		select {
		case <-timer.C:
			c.gotoRoot()
		case <-c.scopeLifetime.Ctx().Done():
			return
		}
	}()
}

func (c *Client[T]) gotoRoot() {
	c.setScope(c.engine.Root())
}

func (c *Client[T]) process(msg parse.Msg, data []byte) {
	scope := c.Scope()
	if scope == nil {
		return
	}

	// TODO(cfoust): 06/29/23 MouseMsg
	key, ok := msg.(parse.KeyMsg)
	if !ok {
		return
	}

	result := scope.Lookup(key.String())
	if result == nil {
		if !scope.options.StickyWrites {
			c.gotoRoot()
		}

		c.events <- RawEvent{
			Data: data,
		}
		return
	}

	c.events <- ActionEvent[T]{
		Action: result.Action,
	}

	if result.Scope != nil {
		c.setScope(result.Scope)
	} else if !scope.options.StickyActions {
		c.gotoRoot()
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
		Lifetime:      util.NewLifetime(e.Ctx()),
		events:        make(chan Event),
		engine:        e,
		scope:         e.Root(),
		scopeLifetime: util.NewLifetime(e.Ctx()),
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
