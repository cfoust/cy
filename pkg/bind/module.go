package bind

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/bind/trie"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/rs/zerolog/log"
	"github.com/sasha-s/go-deadlock"
)

func NewScope[T any]() *trie.Trie[T] {
	return trie.New[T]()
}

type Event interface{}

// An action was triggered
type ActionEvent[T any] struct {
	Action   T
	Source   *trie.Trie[T]
	Sequence []string
}

type Match[T any] struct {
	Bind   trie.Leaf[T]
	Source *trie.Trie[T]
}

// There are partial matches
type PartialEvent[T any] struct {
	Prefix  []string
	Matches []Match[T]
}

// All input that did not match anything in the scope
type RawEvent struct {
	Data []byte
}

// Contains data for a single input event
type input taro.Msg

type Engine[T any] struct {
	deadlock.RWMutex

	in  chan input
	out chan Event

	scopes []*trie.Trie[T]

	// Track the timeout for a user to enter another key
	keyTimeout util.Lifetime

	// Holds the sequence of keys the user has entered
	state []string
}

func NewEngine[T any]() *Engine[T] {
	return &Engine[T]{
		in:         make(chan input),
		out:        make(chan Event, 100),
		keyTimeout: util.NewLifetime(context.Background()),
	}
}

func (e *Engine[T]) Recv() <-chan Event {
	return e.out
}

func (e *Engine[T]) clearTimeout() {
	if !e.keyTimeout.IsDone() {
		e.keyTimeout.Cancel()
	}
}

func (e *Engine[T]) clearState() {
	e.clearTimeout()

	e.Lock()
	e.state = make([]string, 0)
	e.Unlock()

	e.out <- PartialEvent[T]{}
}

func (e *Engine[T]) setState(ctx context.Context, state []string) {
	e.clearTimeout()

	e.Lock()
	e.state = state
	e.keyTimeout = util.NewLifetime(ctx)
	e.Unlock()

	go func() {
		timer := time.NewTimer(1 * time.Second)
		select {
		case <-timer.C:
			e.clearState()
		case <-e.keyTimeout.Ctx().Done():
			return
		}
	}()
}

func (e *Engine[T]) getState() []string {
	e.RLock()
	defer e.RUnlock()
	return e.state
}

func (e *Engine[T]) processKey(ctx context.Context, in input) {
	// Mouse messages have to be translated to match the pane, so we just
	// pass them on
	if _, ok := in.(taro.MouseMsg); ok {
		e.out <- in
		return
	}

	key, ok := in.(taro.KeyMsg)
	if !ok {
		return
	}

	e.RLock()
	state := e.state
	scopes := e.scopes
	e.RUnlock()

	sequence := append(state, key.String())

	// Later scopes override earlier ones
	for i := len(scopes) - 1; i >= 0; i-- {
		scope := scopes[i]
		value, matched := scope.Get(sequence)
		if !matched {
			continue
		}

		// Exact match, let's stop
		e.clearState()
		e.out <- ActionEvent[T]{
			Action:   value,
			Source:   scope,
			Sequence: sequence,
		}
		return
	}

	// Otherwise we might have a partial match
	matches := make([]Match[T], 0)
	for i := len(scopes) - 1; i >= 0; i-- {
		scope := scopes[i]

		for _, match := range scope.Partial(sequence) {
			matches = append(matches, Match[T]{
				Bind:   match,
				Source: scopes[i],
			})
		}
	}

	if len(matches) > 0 {
		e.out <- PartialEvent[T]{
			Prefix:  sequence,
			Matches: matches,
		}
		e.setState(ctx, sequence)
		return
	}

	e.clearState()
	e.out <- in
}

func (e *Engine[T]) SetScopes(scopes ...*trie.Trie[T]) {
	e.clearState()
	e.Lock()
	e.scopes = scopes
	e.Unlock()
}

func (e *Engine[T]) Scopes() []*trie.Trie[T] {
	e.RLock()
	defer e.RUnlock()
	return e.scopes
}

func (e *Engine[T]) Poll(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			return
		case in := <-e.in:
			e.processKey(ctx, in)
		}
	}
}

// Process input and produce events.
func (e *Engine[T]) Input(data []byte) {
	for i, w := 0, 0; i < len(data); i += w {
		var msg taro.Msg
		w, msg = taro.DetectOneMsg(data[i:])
		log.Info().Msgf("%+v %+v", msg, string(data[i:i+w]))
		e.in <- msg
	}
}

func (e *Engine[T]) InputMessage(msg taro.Msg) {
	e.in <- msg
}
