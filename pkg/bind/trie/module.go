package trie

import (
	"fmt"

	"github.com/sasha-s/go-deadlock"
)

// stepChild represents a step and its associated child node or value
type stepChild[T any] struct {
	step Step
	next interface{} // either *Trie[T] or T
}

type Trie[T any] struct {
	deadlock.RWMutex

	// Array of steps and their associated children
	steps []stepChild[T]

	// arbitrary extra data associated with this Trie
	source interface{}
}

// findStepMatches finds all steps that match the given input string
func (t *Trie[T]) findStepMatches(input string) []stepChild[T] {
	matches := make([]stepChild[T], 0)
	for _, step := range t.steps {
		if step.step.Match(input) {
			matches = append(matches, step)
		}
	}
	return matches
}

// findStepByStep finds a step by equality comparison
func (t *Trie[T]) findStepByStep(targetStep Step) *stepChild[T] {
	for i := range t.steps {
		if t.steps[i].step.Equal(targetStep) {
			return &t.steps[i]
		}
	}
	return nil
}

// convertToStep converts an interface{} item to a Step
func convertToStep(item interface{}) Step {
	switch v := item.(type) {
	case string:
		return NewLiteralStep(v)
	case *Regex:
		step, _ := NewRegexStep(v.Pattern)
		return step
	case *Count:
		var pattern Step
		switch p := v.Pattern.(type) {
		case string:
			pattern = NewLiteralStep(p)
		case *Regex:
			pattern, _ = NewRegexStep(p.Pattern)
		default:
			return nil
		}
		return NewCountStep(pattern, v.Min, v.Max)
	default:
		return nil
	}
}

type Leaf[T any] struct {
	Path  []string
	Value T
}

// Leaves gets all of the leaves accessible from this Trie and the path to each leaf.
func (t *Trie[T]) Leaves() (leaves []Leaf[T]) {
	t.RLock()
	defer t.RUnlock()

	for _, step := range t.steps {
		key := t.stepToDisplayKey(step.step)
		switch next := step.next.(type) {
		case T:
			leaves = append(leaves, Leaf[T]{
				Path:  []string{key},
				Value: next,
			})
		case *Trie[T]:
			childLeaves := next.Leaves()
			for _, leaf := range childLeaves {
				newPath := append([]string{key}, leaf.Path...)
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

// stepToDisplayKey converts a Step to a display string for backward compatibility
func (t *Trie[T]) stepToDisplayKey(step Step) string {
	switch s := step.(type) {
	case *LiteralStep:
		return s.Value
	case *RegexStep:
		return fmt.Sprintf("re:%s", s.Pattern)
	case *CountStep:
		var patternStr string
		switch p := s.Pattern.(type) {
		case *LiteralStep:
			patternStr = p.Value
		case *RegexStep:
			patternStr = p.Pattern
		default:
			patternStr = "unknown"
		}
		return fmt.Sprintf("count:%s(%d,%d)", patternStr, s.Min, s.Max)
	default:
		return "unknown"
	}
}

func strToInterface(slice []string) []interface{} {
	query := make([]interface{}, 0)
	for _, step := range slice {
		query = append(query, step)
	}

	return query
}

// Partial gets a list of all partial matches for a sequence, if any.
func (t *Trie[T]) Partial(sequence []string) (result []Leaf[T]) {
	t.RLock()
	defer t.RUnlock()

	return t.getPartialMatches(sequence, 0)
}

// getPartialMatches finds all possible continuations from a given sequence using step-based approach
func (t *Trie[T]) getPartialMatches(sequence []string, seqIndex int) (result []Leaf[T]) {
	if seqIndex >= len(sequence) {
		// We've consumed all input, return all possible continuations
		return t.Leaves()
	}

	step := sequence[seqIndex]

	// Find all steps that match this input
	matches := t.findStepMatches(step)

	for _, match := range matches {
		if node, ok := match.next.(*Trie[T]); ok {
			// Handle CountStep specially for partial matches
			if countStep, ok := match.step.(*CountStep); ok {
				// Try different numbers of repetitions
				for rep := countStep.Min; rep <= countStep.Max; rep++ {
					if seqIndex+rep > len(sequence) {
						// Partial match - check if current subsequence matches
						if rep <= len(sequence)-seqIndex && rep >= countStep.Min {
							allMatch := true
							for i := 0; i < len(sequence)-seqIndex; i++ {
								if !countStep.Pattern.Match(sequence[seqIndex+i]) {
									allMatch = false
									break
								}
							}
							if allMatch {
								result = append(result, node.Leaves()...)
							}
						}
						continue
					}

					// Check if the next 'rep' steps match
					allMatch := true
					for i := 0; i < rep; i++ {
						if !countStep.Pattern.Match(sequence[seqIndex+i]) {
							allMatch = false
							break
						}
					}

					if allMatch {
						result = append(result, node.getPartialMatches(sequence, seqIndex+rep)...)
					}
				}
			} else {
				// Regular step, continue with next index
				result = append(result, node.getPartialMatches(sequence, seqIndex+1)...)
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

	return t.getWithSteps(sequence, 0, make([]interface{}, 0))
}

// getWithSteps is the core state machine logic for step-based matching
func (t *Trie[T]) getWithSteps(sequence []string, seqIndex int, args []interface{}) (value T, finalArgs []interface{}, matched bool) {
	if seqIndex >= len(sequence) {
		// We've consumed all input but haven't found a complete match
		return
	}

	step := sequence[seqIndex]

	// Find all steps that match this input
	matches := t.findStepMatches(step)

	for _, match := range matches {
		switch child := match.next.(type) {
		case T:
			// This is a leaf node - check if we've consumed all input
			if seqIndex == len(sequence)-1 {
				newArgs := args
				// For CountStep, we need special handling to accumulate matched strings
				if countStep, ok := match.step.(*CountStep); ok {
					// For count steps, we need to determine how many repetitions this represents
					repetitions := t.findCountRepetitions(sequence, seqIndex, countStep)
					if repetitions >= countStep.Min && repetitions <= countStep.Max {
						// Collect the matched strings for this count
						matches := sequence[seqIndex : seqIndex+repetitions]
						newArgs = append(args, matches)
						// Check if this consumes all remaining input
						if seqIndex+repetitions == len(sequence) {
							return child, newArgs, true
						}
					}
				} else {
					// For regex steps, add the matched string to args
					if _, ok := match.step.(*RegexStep); ok {
						newArgs = append(args, step)
					}
					return child, newArgs, true
				}
			}
		case *Trie[T]:
			newArgs := args
			nextSeqIndex := seqIndex + 1

			// Handle special logic for CountStep
			if countStep, ok := match.step.(*CountStep); ok {
				repetitions := t.findCountRepetitions(sequence, seqIndex, countStep)
				if repetitions >= countStep.Min && repetitions <= countStep.Max {
					// Collect the matched strings
					matches := sequence[seqIndex : seqIndex+repetitions]
					newArgs = append(args, matches)
					nextSeqIndex = seqIndex + repetitions
				} else {
					continue // This count doesn't match, try next
				}
			} else if _, ok := match.step.(*RegexStep); ok {
				// For regex steps, add the matched string
				newArgs = append(args, step)
			}

			// Recursively try the child trie
			if val, finalArgs, matched := child.getWithSteps(sequence, nextSeqIndex, newArgs); matched {
				return val, finalArgs, true
			}
		}
	}

	return
}

// findCountRepetitions determines how many consecutive matches a CountStep should consume
func (t *Trie[T]) findCountRepetitions(sequence []string, startIndex int, countStep *CountStep) int {
	for rep := countStep.Max; rep >= countStep.Min; rep-- {
		if startIndex+rep > len(sequence) {
			continue
		}

		allMatch := true
		for i := 0; i < rep; i++ {
			if !countStep.Pattern.Match(sequence[startIndex+i]) {
				allMatch = false
				break
			}
		}

		if allMatch {
			return rep
		}
	}
	return 0
}

func (t *Trie[T]) Set(sequence []interface{}, value T) {
	t.Lock()
	defer t.Unlock()

	if len(sequence) == 0 {
		return
	}

	// Navigate to the parent trie, creating nodes as needed
	current := t
	for _, item := range sequence[:len(sequence)-1] {
		step := convertToStep(item)
		if step == nil {
			return // Invalid step
		}

		// Find existing step or create new one
		existing := current.findStepByStep(step)
		if existing != nil {
			if child, ok := existing.next.(*Trie[T]); ok {
				current = child
			} else {
				// Replace non-trie with trie
				newTrie := New[T](nil)
				existing.next = newTrie
				current = newTrie
			}
		} else {
			// Create new child trie
			newTrie := New[T](nil)
			current.steps = append(current.steps, stepChild[T]{
				step: step,
				next: newTrie,
			})
			current = newTrie
		}
	}

	// Handle the final step
	finalStep := convertToStep(sequence[len(sequence)-1])
	if finalStep == nil {
		return
	}

	// Find existing step or create new one
	existing := current.findStepByStep(finalStep)
	if existing != nil {
		existing.next = value
	} else {
		current.steps = append(current.steps, stepChild[T]{
			step: finalStep,
			next: value,
		})
	}
}

func (t *Trie[T]) clear(sequence []interface{}) {
	if len(sequence) == 0 {
		t.steps = make([]stepChild[T], 0)
		return
	}

	// Navigate to parent and remove the final step
	if len(sequence) == 1 {
		// Remove from root
		step := convertToStep(sequence[0])
		if step == nil {
			return
		}

		for i, child := range t.steps {
			if child.step.Equal(step) {
				// Remove this step
				t.steps = append(t.steps[:i], t.steps[i+1:]...)
				break
			}
		}
		return
	}

	// Navigate to the parent
	current := t
	parents := []*Trie[T]{t}
	stepPath := make([]Step, 0, len(sequence))

	for _, item := range sequence[:len(sequence)-1] {
		step := convertToStep(item)
		if step == nil {
			return
		}
		stepPath = append(stepPath, step)

		found := false
		for _, child := range current.steps {
			if child.step.Equal(step) {
				if nextTrie, ok := child.next.(*Trie[T]); ok {
					current = nextTrie
					parents = append(parents, current)
					found = true
					break
				}
			}
		}
		if !found {
			return // Path doesn't exist
		}
	}

	// Remove the final step from the current node
	finalStep := convertToStep(sequence[len(sequence)-1])
	if finalStep == nil {
		return
	}

	for i, child := range current.steps {
		if child.step.Equal(finalStep) {
			current.steps = append(current.steps[:i], current.steps[i+1:]...)
			break
		}
	}

	// Prune empty parents
	for i := len(parents) - 1; i > 0; i-- {
		if len(parents[i].steps) == 0 {
			// Remove this empty node from its parent
			parentStep := stepPath[i-1]
			parent := parents[i-1]
			for j, child := range parent.steps {
				if child.step.Equal(parentStep) {
					parent.steps = append(parent.steps[:j], parent.steps[j+1:]...)
					break
				}
			}
		} else {
			break // Stop pruning if we find a non-empty node
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

	// Find the node to move
	current := t
	for _, item := range from[:len(from)-1] {
		step := convertToStep(item)
		if step == nil {
			return
		}

		found := false
		for _, child := range current.steps {
			if child.step.Equal(step) {
				if nextTrie, ok := child.next.(*Trie[T]); ok {
					current = nextTrie
					found = true
					break
				}
			}
		}
		if !found {
			return // Path doesn't exist
		}
	}

	// Get the final step and its associated data
	finalStep := convertToStep(from[len(from)-1])
	if finalStep == nil {
		return
	}

	var nodeToMove interface{}
	for i, child := range current.steps {
		if child.step.Equal(finalStep) {
			nodeToMove = child.next
			// Remove the old step
			current.steps = append(current.steps[:i], current.steps[i+1:]...)
			break
		}
	}

	if nodeToMove == nil {
		return // Step not found
	}

	// Clear empty parent nodes as needed (reuse clear logic)
	t.clear(from)

	// Navigate to the destination and set the moved node
	current = t
	for _, item := range to[:len(to)-1] {
		step := convertToStep(item)
		if step == nil {
			return
		}

		// Find existing step or create new one
		existing := current.findStepByStep(step)
		if existing != nil {
			if child, ok := existing.next.(*Trie[T]); ok {
				current = child
			} else {
				// Replace non-trie with trie
				newTrie := New[T](nil)
				existing.next = newTrie
				current = newTrie
			}
		} else {
			// Create new child trie
			newTrie := New[T](nil)
			current.steps = append(current.steps, stepChild[T]{
				step: step,
				next: newTrie,
			})
			current = newTrie
		}
	}

	// Set the final step with the moved node
	finalDestStep := convertToStep(to[len(to)-1])
	if finalDestStep == nil {
		return
	}

	// Find existing step or create new one
	existing := current.findStepByStep(finalDestStep)
	if existing != nil {
		existing.next = nodeToMove
	} else {
		current.steps = append(current.steps, stepChild[T]{
			step: finalDestStep,
			next: nodeToMove,
		})
	}
}

func (t *Trie[T]) Source() interface{} {
	return t.source
}

func New[T any](source interface{}) *Trie[T] {
	return &Trie[T]{
		steps:  make([]stepChild[T], 0),
		source: source,
	}
}
