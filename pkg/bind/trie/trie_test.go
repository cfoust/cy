package trie

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestTrie(t *testing.T) {
	trie := New[int]()
	path := []string{
		"one",
		"two",
	}
	trie.Set(path, 2)
	_, matched := trie.Get(path)

	assert.Equal(t, true, matched)
}
