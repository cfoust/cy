package bind

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/bind/trie"
	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/keys"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	"github.com/rs/zerolog/log"
	"github.com/sasha-s/go-deadlock"
)

func NewScope[T any](source interface{}) *trie.Trie[T] {
	return trie.New[T](source)
}

type Event interface{}

// An action was triggered
type ActionEvent[T any] struct {
	Action   T
	Engine   *Engine[T]
	Source   *trie.Trie[T]
	Sequence []string
	// Any regex traversals that matched
	Args []string
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

// Contains data for a single input event
type input taro.Msg

type trackedInput struct {
	input  taro.Msg
	result chan<- bool
}

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

func Run[T any](ctx context.Context, binds *trie.Trie[T]) *Engine[T] {
	engine := NewEngine[T]()
	engine.SetScopes(binds)
	go engine.Poll(ctx)
	return engine
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

func (e *Engine[T]) processKey(ctx context.Context, in input) (consumed bool) {
	// Mouse messages have to be translated to match the pane, so we just
	// pass them on
	if _, ok := in.(taro.MouseMsg); ok {
		e.out <- in
		return
	}

	key, ok := in.(taro.KittyKeyMsg)
	if !ok {
		return
	}

	// For now, we only support legacy keys
	if _, canEncode := keys.Key(key).Bytes(emu.DefaultMode, emu.KeyLegacy); !canEncode {
		e.out <- in
		return
	}

	e.RLock()
	state := e.state
	scopes := e.scopes
	e.RUnlock()

	sequence := append(state, keys.Key(key).String())

	// Later scopes override earlier ones
	for i := len(scopes) - 1; i >= 0; i-- {
		scope := scopes[i]
		value, re, matched := scope.Get(sequence)
		if !matched {
			continue
		}

		// Exact match, let's stop
		consumed = true
		e.clearState()
		e.out <- ActionEvent[T]{
			Action:   value,
			Engine:   e,
			Source:   scope,
			Sequence: sequence,
			Args:     re,
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
		consumed = true
		e.out <- PartialEvent[T]{
			Prefix:  sequence,
			Matches: matches,
		}
		e.setState(ctx, sequence)
		return
	}

	e.clearState()
	e.out <- in
	return
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
			if tracked, ok := in.(trackedInput); ok {
				tracked.result <- e.processKey(
					ctx,
					tracked.input,
				)
				continue
			}
			e.processKey(ctx, in)
		}
	}
}

// Process input and produce events.
func (e *Engine[T]) Input(data []byte) {
	for i, w := 0, 0; i < len(data); i += w {
		var msg taro.Msg
		msg, w = taro.DetectOneMsg(data[i:])
		if _, ok := msg.(taro.KittyKeyMsg); ok {
			log.Info().
				Str("type", "input").
				Bytes("bytes", data[i:]).
				Msgf("input key: %#v", msg)
		}
		e.in <- msg
	}
}

// InputMessage directly passes a taro.Msg (which is typically a taro.KeyMsg)
// into the binding engine. If the key was "consumed", or produced either a
// partial or full match, InputMessage returns true.
func (e *Engine[T]) InputMessage(msg taro.Msg) (consumed bool) {
	out := make(chan bool)
	e.in <- trackedInput{
		input:  msg,
		result: out,
	}

	return <-out
}
