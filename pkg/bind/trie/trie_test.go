package trie

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func re(pattern string) *Regex {
	r, err := NewRegex(pattern)
	if err != nil {
		panic(err)
	}

	return r
}

func TestTrie(t *testing.T) {
	trie := New[int]()
	trie.Set([]interface{}{
		"one",
		"two",
	}, 2)

	_, matched := trie.Get([]string{
		"one",
		"two",
	})
	require.Equal(t, true, matched)

	trie.Set([]interface{}{
		"one",
		"three",
	}, 1)

	trie.Set([]interface{}{
		"two",
	}, 1)

	require.Equal(t, 3, len(trie.Leaves()))
	require.Equal(t, 2, len(trie.Partial([]string{
		"one",
	})))
}

func TestRegex(t *testing.T) {
	trie := New[int]()
	trie.Set([]interface{}{
		re("[abc]"),
		"t",
	}, 2)

	_, matched := trie.Get([]string{
		"a",
		"t",
	})
	require.Equal(t, true, matched)

	_, matched = trie.Get([]string{
		"d",
		"t",
	})
	require.Equal(t, false, matched)
}
