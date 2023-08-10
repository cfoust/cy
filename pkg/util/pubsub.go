package util

import (
	"github.com/sasha-s/go-deadlock"
)

type Publisher[T any] struct {
	subscribers map[chan T]struct{}
	mutex       deadlock.Mutex
}

func NewPublisher[T any]() *Publisher[T] {
	return &Publisher[T]{
		subscribers: make(map[chan T]struct{}),
	}
}

func (t *Publisher[T]) Publish(value T) {
	t.mutex.Lock()
	subscribers := t.subscribers
	t.mutex.Unlock()

	for subscriber := range subscribers {
		subscriber <- value
	}
}

type Subscriber[T any] struct {
	channel chan T
	topic   *Publisher[T]
}

func (t *Publisher[T]) Subscribe() *Subscriber[T] {
	channel := make(chan T)
	t.mutex.Lock()
	t.subscribers[channel] = struct{}{}
	t.mutex.Unlock()

	return &Subscriber[T]{channel, t}
}

func (t *Subscriber[T]) Recv() <-chan T {
	return t.channel
}

func (t *Subscriber[T]) Done() {
	topic := t.topic
	topic.mutex.Lock()
	delete(topic.subscribers, t.channel)
	topic.mutex.Unlock()
}
