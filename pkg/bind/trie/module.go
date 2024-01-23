package trie

import (
	"fmt"

	"github.com/sasha-s/go-deadlock"
)

type Trie[T any] struct {
	deadlock.RWMutex

	// mapping from exact match -> node or T
	next map[string]interface{}

	// a mapping from raw regex pattern -> node or T
	nextRe map[string]*Regex
}

func (t *Trie[T]) resolve(key interface{}) (value interface{}, matched bool, regex bool) {
	switch key := key.(type) {
	case string:
		// try matching against a regex
		for _, re := range t.nextRe {
			if !re.compiled.MatchString(key) {
				continue
			}

			return re.next, true, true
		}

		node, ok := t.next[key]
		if !ok {
			return nil, false, false
		}
		return node, true, false
	case *Regex:
		node, ok := t.nextRe[key.Pattern]
		if !ok {
			return nil, false, false
		}
		return node.next, true, false
	}

	return nil, false, false
}

func (t *Trie[T]) getParent(key interface{}) *Trie[T] {
	node, ok, _ := t.resolve(key)
	if !ok {
		return nil
	}

	if parent, ok := node.(*Trie[T]); ok {
		return parent
	}

	return nil
}

func (t *Trie[T]) getLeaf(key string) (value T, ok bool) {
	node, found, _ := t.resolve(key)
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

func (t *Trie[T]) access(sequence []interface{}, shouldCreate bool) *Trie[T] {
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

		if shouldCreate {
			switch step := step.(type) {
			case string:
				current.next[step] = next
			case *Regex:
				step.next = next
				current.nextRe[step.Pattern] = step
			}
		}

		current = next
	}

	return current
}

type Leaf[T any] struct {
	Path  []string
	Value T
}

type location struct {
	Key  string
	Next interface{}
}

// Leaves gets all of the leaves accessible from this Trie and the path to each
// leaf. Regex paths are represented as "re:[pattern]".
func (t *Trie[T]) Leaves() (leaves []Leaf[T]) {
	t.RLock()
	defer t.RUnlock()

	locations := make([]location, 0)
	for key, next := range t.next {
		locations = append(locations, location{
			Key:  key,
			Next: next,
		})
	}

	for _, re := range t.nextRe {
		locations = append(locations, location{
			Key:  fmt.Sprintf("re:%s", re.Pattern),
			Next: re.next,
		})
	}

	for _, loc := range locations {
		switch next := loc.Next.(type) {
		case T:
			leaves = append(
				leaves,
				Leaf[T]{
					Path: []string{
						loc.Key,
					},
					Value: next,
				},
			)
		case *Trie[T]:
			childLeaves := next.Leaves()
			for _, leaf := range childLeaves {
				newPath := append(
					[]string{
						loc.Key,
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
	}

	return
}

func strToInterface(slice []string) []interface{} {
	query := make([]interface{}, 0)
	for _, step := range slice {
		query = append(query, step)
	}

	return query
}

// Get a list of all partial matches for a sequence, if any.
func (t *Trie[T]) Partial(sequence []string) (result []Leaf[T]) {
	t.RLock()
	defer t.RUnlock()

	parent := t.access(strToInterface(sequence), false)
	if parent == nil {
		return
	}

	return parent.Leaves()
}

// Get attempts to retrieve the leaf referred to by `sequence`. Any steps
// traversed using regex matches will be returned in `re` in the order in which
// they appeared in the sequence.
func (t *Trie[T]) Get(sequence []string) (value T, re []string, matched bool) {
	t.RLock()
	defer t.RUnlock()

	var current *Trie[T] = t
	for i, step := range sequence {
		node, ok, isRegex := current.resolve(step)
		if !ok {
			return
		}

		if isRegex {
			re = append(re, step)
		}
		switch node := node.(type) {
		case T:
			if i != len(sequence)-1 {
				return
			}
			value = node
			matched = true
			return
		case *Trie[T]:
			current = node
		}

	}

	return
}

func (t *Trie[T]) Set(sequence []interface{}, value T) {
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

	switch step := last.(type) {
	case string:
		parent.next[step] = value
	case *Regex:
		parent.nextRe[step.Pattern] = step
		step.next = value
	}
}

// Clear all mappings in the trie with the prefix `sequence`.
func (t *Trie[T]) Clear(sequence []interface{}) {
	t.Lock()
	defer t.Unlock()

	if len(sequence) == 0 {
		return
	}

	// First, delete the portion of the tree that this sequence refers to
	lastIndex := len(sequence) - 1
	last := sequence[lastIndex]
	parent := t.access(sequence[:lastIndex], false)
	if parent == nil {
		return
	}

	switch step := last.(type) {
	case string:
		delete(parent.next, step)
	case *Regex:
		delete(parent.nextRe, step.Pattern)
	}

	// Then prune any portions of the tree that no longer have any leaves
	// For example:
	// a -> b -> c
	// |    |    |
	// |    |    leaf
	// |    parent
	// parent
	// we've already cleared b's leaves, we need to check whether b has any
	// other keys, if it does not, remove it from a
	for i := len(sequence) - 3; i >= 0; i-- {
		parent := t.access(sequence[:i+1], false)
		child := t.access(sequence[:i+2], false)
		if parent == nil || child == nil {
			return
		}

		numLeaves := len(child.next) + len(child.nextRe)
		if numLeaves != 0 {
			break
		}

		switch step := sequence[i].(type) {
		case string:
			delete(parent.next, step)
		case *Regex:
			delete(parent.nextRe, step.Pattern)
		}
	}
}

func New[T any]() *Trie[T] {
	return &Trie[T]{
		next:   make(map[string]interface{}),
		nextRe: make(map[string]*Regex),
	}
}
