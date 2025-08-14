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

	// a mapping for count patterns - keyed by a unique identifier
	nextCount map[string]*Count

	// arbitrary extra data associated with this Trie
	source interface{}
}

func (t *Trie[T]) resolve(
	key interface{},
) (value interface{}, matched bool, regex bool) {
	switch key := key.(type) {
	case string:
		// try matching against count patterns
		for _, count := range t.nextCount {
			if count.Matches(key) {
				return count.next, true, true
			}
		}
		
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
	case *Count:
		// For Count patterns, we need a unique key to identify this count instance
		countKey := fmt.Sprintf("count:%p", key)
		node, ok := t.nextCount[countKey]
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

func (t *Trie[T]) access(sequence []interface{}, shouldCreate bool) *Trie[T] {
	if len(sequence) == 0 {
		return t
	}

	current := t
	for _, step := range sequence {
		next := current.getParent(step)
		if next == nil {
			if !shouldCreate {
				return nil
			}
			next = New[T](nil)
		}

		if shouldCreate {
			switch step := step.(type) {
			case string:
				current.next[step] = next
			case *Regex:
				step.next = next
				current.nextRe[step.Pattern] = step
			case *Count:
				countKey := fmt.Sprintf("count:%p", step)
				step.next = next
				current.nextCount[countKey] = step
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

	for _, count := range t.nextCount {
		var patternStr string
		switch p := count.Pattern.(type) {
		case string:
			patternStr = p
		case *Regex:
			patternStr = p.Pattern
		default:
			patternStr = "unknown"
		}
		locations = append(locations, location{
			Key:  fmt.Sprintf("count:%s(%d,%d)", patternStr, count.Min, count.Max),
			Next: count.next,
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

	// Use the new partial matching logic that handles Count patterns
	return t.getPartialMatches(sequence, 0)
}

// getPartialMatches finds all possible continuations from a given sequence
func (t *Trie[T]) getPartialMatches(sequence []string, seqIndex int) (result []Leaf[T]) {
	if seqIndex >= len(sequence) {
		// We've consumed all input, return all possible continuations
		return t.Leaves()
	}

	step := sequence[seqIndex]

	// Try exact string matches first
	if next, ok := t.next[step]; ok {
		if node, ok := next.(*Trie[T]); ok {
			result = append(result, node.getPartialMatches(sequence, seqIndex+1)...)
		}
	}

	// Try regex matches
	for _, re := range t.nextRe {
		if re.compiled.MatchString(step) {
			if node, ok := re.next.(*Trie[T]); ok {
				result = append(result, node.getPartialMatches(sequence, seqIndex+1)...)
			}
		}
	}

	// Try count patterns
	for _, count := range t.nextCount {
		// Try different numbers of repetitions (min to max)
		for rep := count.Min; rep <= count.Max; rep++ {
			// Check if we have enough remaining sequence for this repetition
			if seqIndex+rep > len(sequence) {
				// Not enough sequence for this repetition count,
				// but we might have a partial match if we're at min or above
				if rep <= len(sequence)-seqIndex && rep >= count.Min {
					// Check if what we have so far matches
					allMatch := true
					for i := 0; i < len(sequence)-seqIndex; i++ {
						if !count.Matches(sequence[seqIndex+i]) {
							allMatch = false
							break
						}
					}
					if allMatch {
						// This is a partial match for the count pattern
						if node, ok := count.next.(*Trie[T]); ok {
							result = append(result, node.Leaves()...)
						}
					}
				}
				continue
			}

			// Check if the next 'rep' steps match the count pattern
			allMatch := true
			for i := 0; i < rep; i++ {
				if !count.Matches(sequence[seqIndex+i]) {
					allMatch = false
					break
				}
			}

			if allMatch {
				if node, ok := count.next.(*Trie[T]); ok {
					result = append(result, node.getPartialMatches(sequence, seqIndex+rep)...)
				}
			}
		}
	}

	return
}

// Get attempts to retrieve the leaf referred to by `sequence`. Any steps
// traversed using regex matches will be returned as strings, and count patterns
// will be returned as []string, all in the order they appeared in the sequence.
func (t *Trie[T]) Get(sequence []string) (value T, args []interface{}, matched bool) {
	t.RLock()
	defer t.RUnlock()

	// This is complex because we need to handle Count patterns specially
	// We'll use a recursive helper function to try different paths
	value, args, matched = t.getWithCounts(sequence, 0, make([]interface{}, 0))
	return
}

// getWithCounts recursively tries to match the sequence, handling Count patterns
func (t *Trie[T]) getWithCounts(sequence []string, seqIndex int, args []interface{}) (value T, finalArgs []interface{}, matched bool) {
	if seqIndex >= len(sequence) {
		// We've consumed all input, this means we're done with the sequence
		// but we haven't matched any value yet. This is not a complete match.
		return // No complete match found
	}

	step := sequence[seqIndex]

	// Try exact string matches first
	if next, ok := t.next[step]; ok {
		switch node := next.(type) {
		case T:
			if seqIndex == len(sequence)-1 {
				return node, args, true
			}
		case *Trie[T]:
			return node.getWithCounts(sequence, seqIndex+1, args)
		}
	}

	// Try regex matches
	for _, re := range t.nextRe {
		if re.compiled.MatchString(step) {
			newArgs := append(args, step)
			switch next := re.next.(type) {
			case T:
				if seqIndex == len(sequence)-1 {
					return next, newArgs, true
				}
			case *Trie[T]:
				if val, finalArgs, matched := next.getWithCounts(sequence, seqIndex+1, newArgs); matched {
					return val, finalArgs, true
				}
			}
		}
	}

	// Try count patterns - this is the most complex part
	for _, count := range t.nextCount {
		// Try different numbers of repetitions (min to max)
		for rep := count.Min; rep <= count.Max; rep++ {
			// Check if we have enough remaining sequence for this repetition
			if seqIndex+rep > len(sequence) {
				continue
			}

			// Check if the next 'rep' steps match the count pattern
			matches := make([]string, 0, rep)
			allMatch := true
			for i := 0; i < rep; i++ {
				if !count.Matches(sequence[seqIndex+i]) {
					allMatch = false
					break
				}
				matches = append(matches, sequence[seqIndex+i])
			}

			if allMatch {
				newArgs := append(args, matches)
				switch next := count.next.(type) {
				case T:
					if seqIndex+rep == len(sequence) {
						return next, newArgs, true
					}
				case *Trie[T]:
					if val, finalArgs, matched := next.getWithCounts(sequence, seqIndex+rep, newArgs); matched {
						return val, finalArgs, true
					}
				}
			}
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
	case *Count:
		countKey := fmt.Sprintf("count:%p", step)
		parent.nextCount[countKey] = step
		step.next = value
	}
}

func (t *Trie[T]) clear(sequence []interface{}) {
	if len(sequence) == 0 {
		t.next = make(map[string]interface{})
		t.nextRe = make(map[string]*Regex)
		t.nextCount = make(map[string]*Count)
		return
	}

	// First, delete the portion of the tree that this sequence refers to
	{
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
		case *Count:
			countKey := fmt.Sprintf("count:%p", step)
			delete(parent.nextCount, countKey)
		}
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

		numLeaves := len(child.next) + len(child.nextRe) + len(child.nextCount)
		if numLeaves != 0 {
			break
		}

		switch step := sequence[i].(type) {
		case string:
			delete(parent.next, step)
		case *Regex:
			delete(parent.nextRe, step.Pattern)
		case *Count:
			countKey := fmt.Sprintf("count:%p", step)
			delete(parent.nextCount, countKey)
		}
	}
}

// Clear all mappings in the trie with the prefix `sequence`. An empty sequence
// will clear all mappings.
func (t *Trie[T]) Clear(sequence []interface{}) {
	t.Lock()
	defer t.Unlock()

	t.clear(sequence)
}

// Remap the subtree at sequence `from` to sequence `to`.
func (t *Trie[T]) Remap(from, to []interface{}) {
	t.Lock()
	defer t.Unlock()

	if len(from) == 0 || len(to) == 0 {
		return
	}

	oldParent := t.access(from[:len(from)-1], false)
	if oldParent == nil {
		return
	}

	// Get the node or leaf that this step referred to
	var next interface{}
	switch step := from[len(from)-1].(type) {
	case string:
		next = oldParent.next[step]
	case *Regex:
		next = oldParent.nextRe[step.Pattern].next
	case *Count:
		countKey := fmt.Sprintf("count:%p", step)
		next = oldParent.nextCount[countKey].next
	}

	if next == nil {
		return
	}

	// Remove that path
	oldParent.clear(from)

	// Then create the path to the new parent if necessary
	newParent := t.access(from[:len(to)-1], true)
	if newParent == nil {
		// can't happen, we're creating
		return
	}

	switch step := to[len(to)-1].(type) {
	case string:
		newParent.next[step] = next
	case *Regex:
		step.next = next
		newParent.nextRe[step.Pattern] = step
	case *Count:
		countKey := fmt.Sprintf("count:%p", step)
		step.next = next
		newParent.nextCount[countKey] = step
	}
}

func (t *Trie[T]) Source() interface{} {
	return t.source
}

func New[T any](source interface{}) *Trie[T] {
	return &Trie[T]{
		next:      make(map[string]interface{}),
		nextRe:    make(map[string]*Regex),
		nextCount: make(map[string]*Count),
		source:    source,
	}
}
