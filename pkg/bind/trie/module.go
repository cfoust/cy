package trie

type Trie[T any] struct {
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

type Match[T any] struct {
	Partial  bool
	Sequence []string
	Value    T
}

func (t *Trie[T]) Get(sequence []string) (value T, matched bool) {
	var current *Trie[T] = t
	for i, step := range sequence {
		if (len(sequence) - 1) == i {
			leaf, ok := current.getLeaf(step)
			if !ok {
				return
			}

			matched = true
			value = leaf
			return
		}

		var next *Trie[T] = current.getParent(step)
		if next == nil {
			return
		}
		current = next
	}
	return
}

func (t *Trie[T]) Set(sequence []string, value T) {
	var current *Trie[T] = t
	for i, step := range sequence {
		if (len(sequence) - 1) == i {
			current.next[step] = value
			continue
		}

		var next *Trie[T] = current.getParent(step)
		if next == nil {
			next = New[T]()
		}

		current.next[step] = next
		current = next
	}
}

func New[T any]() *Trie[T] {
	return &Trie[T]{
		next: make(map[string]interface{}),
	}
}
