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
	p.m.Lock()
	subscribers := p.subscribers
	p.m.Unlock()

	for subscriber := range subscribers {
		subscriber.m.RLock()
		active := subscriber.active
		subscriber.m.RUnlock()
		if !active {
			continue
		}

		select {
		case <-subscriber.Ctx().Done():
			subscriber.Done()
		case subscriber.channel <- value:
		}
	}
}

type Subscriber[T any] struct {
	Lifetime
	active    bool
	channel   chan T
	publisher *Publisher[T]
	m         deadlock.RWMutex
}

func (t *Publisher[T]) Subscribe(ctx context.Context) *Subscriber[T] {
	channel := make(chan T)
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
