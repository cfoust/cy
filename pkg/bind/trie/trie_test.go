package trie

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestTrie(t *testing.T) {
	trie := New[int]()
	trie.Set([]string{
		"one",
		"two",
	}, 2)

	_, matched := trie.Get([]string{
		"one",
		"two",
	})
	assert.Equal(t, true, matched)

	trie.Set([]string{
		"one",
		"three",
	}, 1)

	trie.Set([]string{
		"two",
	}, 1)

	assert.Equal(t, 3, len(trie.Leaves()))
	assert.Equal(t, 2, len(trie.Partial([]string{
		"one",
	})))
}
