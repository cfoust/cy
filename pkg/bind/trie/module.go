package trie

type leaf[T any] struct {
	Value T
}

type Trie[T any] struct {
	next map[string]interface{}
}

func isLeaf[T any](value interface{}) bool {
	_, ok := (value).(*leaf[T])
	return ok
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

func (t *Trie[T]) Set(sequence []string, value T) {
	var current *Trie[T] = t
	for i, step := range sequence {
		if (len(sequence) - 1) == i {
			current.next[step] = leaf[T]{
				Value: value,
			}
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
