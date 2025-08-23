package trie

import (
	"slices"
)

// trieNode represents a step and its associated Trie or value
type trieNode[T any] struct {
	step Step

	// either *Trie[T] or T
	next any
}

func (t *trieNode[T]) F() {
}

type Trie[T any] []*trieNode[T]

func stepOrder(s Step) int {
	switch s.(type) {
	case *LiteralStep:
		return 0
	case *RegexStep:
		return 1
	case *CountStep:
		return 2
	}

	return -1
}

// successors gets all of the children accessible from this node.
func (t *Trie[T]) successors() (successors []*trieNode[T]) {
	// Count steps can be skipped if they have a Min of zero, so we need
	// to resolve them if that's the case.
	for _, child := range *t {
		successors = append(successors, child)

		count, ok := child.step.(*CountStep)
		if !ok || count.Min != 0 {
			continue
		}

		countTrie, ok := child.next.(*Trie[T])
		if !ok {
			continue
		}

		successors = append(
			successors,
			countTrie.successors()...,
		)
	}
	return
}

// next determines whether `input` matches any successor node or nil if it does
// not.
func (t *Trie[T]) next(input string) *trieNode[T] {
	var matches []*trieNode[T]
	for _, child := range t.successors() {
		if !child.step.Match(input) {
			continue
		}

		matches = append(matches, child)
	}

	if len(matches) == 0 {
		return nil
	}

	// Impose an ordering on steps since they can conflict
	slices.SortFunc(matches, func(a, b *trieNode[T]) int {
		return stepOrder(a.step) - stepOrder(b.step)
	})

	return matches[0]
}

func (t *Trie[T]) nextStep(targetStep Step) *trieNode[T] {
	for i := range *t {
		if !(*t)[i].step.Equal(targetStep) {
			continue
		}

		return (*t)[i]
	}

	return nil
}

// resolve performs the sequence of inputs. result is the *Trie that the state
// machine ended on or the value the sequence resolved to. done is true if all
// inputs were used.
func (t *Trie[T]) resolve(sequence []string) (
	result any,
	captures [][]string,
	done bool,
) {
	if len(sequence) == 0 {
		return
	}

	node := t.next(sequence[0])
	if node == nil {
		return
	}

	var (
		consumed       = sequence[:1]
		count, isCount = node.step.(*CountStep)
	)
	if isCount {
		var i int = 1 // already have one
		for ; i < min(len(sequence), count.Max); i++ {
			if !count.Match(sequence[i]) {
				break
			}
		}

		// Not enough to satisfy this CountStep
		// We cannot continue
		if i < count.Min {
			return
		}

		consumed = sequence[:i]
	}

	result = node.next
	captures = append(captures, consumed)
	done = len(sequence)-len(consumed) == 0
	nextTrie, isTrie := result.(*Trie[T])
	if done || !isTrie {
		return
	}

	// Descend down the tree
	var rest [][]string
	result, rest, done = nextTrie.resolve(sequence[len(consumed):])
	captures = append(captures, rest...)
	return
}

func (t *Trie[T]) set(sequence []Step, value any) {
	if len(sequence) == 0 {
		return
	}

	var (
		head     = sequence[0]
		isLast   = len(sequence) == 1
		node     = t.nextStep(head)
		nextTrie *Trie[T]
	)
	if node == nil {
		nextTrie = New[T]()
		node = &trieNode[T]{step: head, next: nextTrie}
		(*t) = append(*t, node)
	}

	// Node now definitely exists--if it's a leaf, we can stop here
	if isLast {
		node.next = value
		return
	}

	// It's not a leaf
	nextTrie, isTrie := node.next.(*Trie[T])
	if !isTrie {
		nextTrie = New[T]()
	}

	node.next = nextTrie
	nextTrie.set(sequence[1:], value)
}

func (t *Trie[T]) Set(sequence []Step, value T) {
	t.set(sequence, value)
}

// Get attempts to retrieve the leaf referred to by sequence. captures contains
// the parts of sequence matched by each intermediate step. ok is true if this
// sequence resolved to a leaf.
func (t *Trie[T]) Get(sequence []string) (
	value T,
	captures [][]string,
	ok bool,
) {
	var (
		node any
		done bool
	)
	node, captures, done = t.resolve(sequence)
	if !done {
		return
	}

	value, ok = node.(T)
	return
}

type Leaf[T any] struct {
	Path  []Step
	Value T
}

// Leaves gets all of the leaves accessible from this Trie and the path to each leaf.
func (t *Trie[T]) Leaves() (leaves []Leaf[T]) {
	for _, child := range *t {
		switch next := child.next.(type) {
		case T:
			leaves = append(leaves, Leaf[T]{
				Path:  []Step{child.step},
				Value: next,
			})
		case *Trie[T]:
			childLeaves := next.Leaves()
			for _, leaf := range childLeaves {
				newLeaf := Leaf[T]{
					Path: append(
						[]Step{child.step},
						leaf.Path...,
					),
					Value: leaf.Value,
				}
				leaves = append(leaves, newLeaf)
			}
		}
	}

	return
}

// Partial gets a list of all partial matches for a sequence, if any.
func (t *Trie[T]) Partial(sequence []string) (result []Leaf[T]) {
	node, _, _ := t.resolve(sequence)
	if node == nil {
		return
	}

	trie, ok := node.(*Trie[T])
	if !ok {
		return
	}

	return trie.Leaves()
}

func (t *Trie[T]) access(path []Step) (node *Trie[T]) {
	if len(path) == 0 {
		return t
	}

	var (
		next         = t.nextStep(path[0])
		trie, isTrie = next.next.(*Trie[T])
	)
	if !isTrie {
		return nil
	}
	if len(path) == 1 {
		return trie
	}

	return trie.access(path[1:])
}

func (t *Trie[T]) delete(step Step) {
	newNodes := make([]*trieNode[T], 0)
	for _, node := range *t {
		if node.step.Equal(step) {
			continue
		}
		newNodes = append(newNodes, node)
	}

	// Nothing changed
	if len(newNodes) == len(*t) {
		return
	}

	(*t) = newNodes
}

func (t *Trie[T]) clear(sequence []Step) {
	if len(sequence) == 0 {
		// Clear all
		(*t) = make([]*trieNode[T], 0)
		return
	}

	// First, delete the portion of the tree that this sequence refers to
	{
		var (
			lastIndex = len(sequence) - 1
			last      = sequence[lastIndex]
			parent    = t.access(sequence[:lastIndex])
		)
		if parent == nil {
			return
		}

		parent.delete(last)
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
		var (
			parent = t.access(sequence[:i+1])
			child  = t.access(sequence[:i+2])
		)
		if parent == nil || child == nil {
			return
		}

		if len(*child) != 0 {
			break
		}

		parent.delete(sequence[i])
	}
}

// Clear all mappings in the trie with the prefix `sequence`. An empty sequence
// will clear all mappings.
func (t *Trie[T]) Clear(sequence []Step) {
	t.clear(sequence)
}

// Remap the subtree at sequence `from` to sequence `to`.
func (t *Trie[T]) Remap(from, to []Step) {
	if len(from) == 0 || len(to) == 0 {
		return
	}

	oldParent := t.access(from[:len(from)-1])
	if oldParent == nil {
		return
	}

	oldTrieNode := oldParent.nextStep(from[len(from)-1])
	if oldTrieNode == nil {
		return
	}

	// Get the node or leaf that this step referred to
	var next = oldTrieNode.next
	if next == nil {
		return
	}

	t.clear(from)
	t.set(to, next)
}

func New[T any]() *Trie[T] {
	t := Trie[T](make([]*trieNode[T], 0))
	return &t
}
