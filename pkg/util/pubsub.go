package util

import (
	"context"

	"github.com/sasha-s/go-deadlock"
)

type Publisher[T any] struct {
	subscribers map[*Subscriber[T]]struct{}
	m           deadlock.RWMutex
}

func NewPublisher[T any]() *Publisher[T] {
	return &Publisher[T]{
		subscribers: make(map[*Subscriber[T]]struct{}),
	}
}

func (p *Publisher[T]) Publish(value T) {
	p.m.RLock()
	for subscriber := range p.subscribers {
		subscriber.m.RLock()
		active := subscriber.active
		subscriber.m.RUnlock()
		if !active {
			continue
		}

		// Non-blocking send: if the subscriber isn't keeping up,
		// skip this update. This is safe because subscribers
		// re-read full state on wakeup anyway.
		select {
		case subscriber.channel <- value:
		default:
		}
	}
	p.m.RUnlock()
}

type Subscriber[T any] struct {
	Lifetime
	active    bool
	channel   chan T
	publisher *Publisher[T]
	m         deadlock.RWMutex
}

func (t *Publisher[T]) Subscribe(ctx context.Context) *Subscriber[T] {
	channel := make(chan T, 1)
	s := &Subscriber[T]{
		Lifetime:  NewLifetime(ctx),
		channel:   channel,
		publisher: t,
		active:    true,
	}

	t.m.Lock()
	t.subscribers[s] = struct{}{}
	t.m.Unlock()

	return s
}

func (t *Subscriber[T]) Recv() <-chan T {
	return t.channel
}

func (t *Subscriber[T]) Done() {
	t.m.Lock()
	active := t.active
	if active {
		t.active = false
	}
	t.m.Unlock()

	if !active {
		return
	}

	publisher := t.publisher
	publisher.m.Lock()
	delete(publisher.subscribers, t)
	publisher.m.Unlock()
}
