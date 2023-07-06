package trie

import (
	"github.com/sasha-s/go-deadlock"
)

type Trie[T any] struct {
	deadlock.RWMutex
	next map[string]interface{}
}

func (t *Trie[T]) getParent(key string) *Trie[T] {
	node, ok := t.next[key]
	if !ok {
		return nil
	}

	if parent, ok := node.(*Trie[T]); ok {
		return parent
	}

	return nil
}

func (t *Trie[T]) getLeaf(key string) (value T, ok bool) {
	node, found := t.next[key]
	if !found {
		return
	}

	if leaf, cast := node.(T); cast {
		value = leaf
		ok = true
		return
	}

	return
}

func (t *Trie[T]) access(sequence []string, shouldCreate bool) *Trie[T] {
	if len(sequence) == 0 {
		return t
	}

	var current *Trie[T] = t
	for _, step := range sequence {
		var next *Trie[T] = current.getParent(step)
		if next == nil {
			if !shouldCreate {
				return nil
			}
			next = New[T]()
		}

		current.next[step] = next
		current = next
	}

	return current
}

type Leaf[T any] struct {
	Path  []string
	Value T
}

func (t *Trie[T]) Leaves() (leaves []Leaf[T]) {
	t.RLock()
	defer t.RUnlock()

	for key := range t.next {
		if leaf, ok := t.getLeaf(key); ok {
			leaves = append(
				leaves,
				Leaf[T]{
					Path: []string{
						key,
					},
					Value: leaf,
				},
			)
			continue
		}

		parent := t.getParent(key)
		if parent == nil {
			continue
		}

		childLeaves := parent.Leaves()
		for _, leaf := range childLeaves {
			newPath := append(
				[]string{
					key,
				},
				leaf.Path...,
			)
			newLeaf := Leaf[T]{
				Path:  newPath,
				Value: leaf.Value,
			}
			leaves = append(leaves, newLeaf)
		}
	}

	return
}

// Get a list of all partial matches for a sequence, if any.
func (t *Trie[T]) Partial(sequence []string) (result []Leaf[T]) {
	t.RLock()
	defer t.RUnlock()

	parent := t.access(sequence, false)
	if parent == nil {
		return
	}

	return parent.Leaves()
}

func (t *Trie[T]) Get(sequence []string) (value T, matched bool) {
	t.RLock()
	defer t.RUnlock()

	lastIndex := len(sequence) - 1
	last := sequence[lastIndex]
	parent := t.access(sequence[:lastIndex], false)
	if parent == nil {
		return
	}

	leaf, ok := parent.getLeaf(last)
	if !ok {
		return
	}

	matched = true
	value = leaf
	return
}

func (t *Trie[T]) Set(sequence []string, value T) {
	t.Lock()
	defer t.Unlock()

	if len(sequence) == 0 {
		return
	}

	lastIndex := len(sequence) - 1
	last := sequence[lastIndex]
	parent := t.access(sequence[:lastIndex], true)
	if parent == nil {
		// can't happen
		return
	}
	parent.next[last] = value
}

func New[T any]() *Trie[T] {
	return &Trie[T]{
		next: make(map[string]interface{}),
	}
}
