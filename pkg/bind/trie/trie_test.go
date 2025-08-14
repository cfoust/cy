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
	trie := New[int](nil)
	trie.Set([]interface{}{
		"one",
		"two",
	}, 2)

	_, _, matched := trie.Get([]string{
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

	// The tree:
	// one -> two -> 2
	// one -> three -> 1
	// two -> 1
	require.Equal(t, 3, len(trie.Leaves()))
	require.Equal(t, 2, len(trie.Partial([]string{
		"one",
	})))

	trie.Remap([]interface{}{
		"one",
	}, []interface{}{
		"three",
	})
	// The tree:
	// three -> two -> 2
	// three -> three -> 1
	// two -> 1
	require.Equal(t, 3, len(trie.Leaves()))
	require.Equal(t, 2, len(trie.Partial([]string{
		"three",
	})))

	trie.Clear([]interface{}{
		"three",
	})
	// The tree:
	// two -> 1
	require.Equal(t, 1, len(trie.Leaves()))
	require.Equal(t, 0, len(trie.Partial([]string{
		"one",
	})))

	trie.Clear([]interface{}{})
	require.Equal(t, 0, len(trie.Leaves()))
}

func TestManyRemap(t *testing.T) {
	trie := New[int](nil)

	trie.Set([]interface{}{
		"one",
		"three",
	}, 1)

	trie.Remap([]interface{}{
		"one",
	}, []interface{}{
		"three",
	})

	require.Equal(t, 1, len(trie.Leaves()))

	trie.Remap([]interface{}{
		"one",
	}, []interface{}{
		"three",
	})
	require.Equal(t, 1, len(trie.Leaves()))
}

func TestRegex(t *testing.T) {
	trie := New[int](nil)
	trie.Set([]interface{}{
		re("[abc]"),
		"t",
	}, 2)

	_, args, matched := trie.Get([]string{
		"a",
		"t",
	})
	require.Equal(t, []interface{}{"a"}, args)
	require.Equal(t, true, matched)

	_, _, matched = trie.Get([]string{
		"d",
		"t",
	})
	require.Equal(t, false, matched)
}

func TestMultipleRegex(t *testing.T) {
	trie := New[int](nil)
	trie.Set([]interface{}{
		re("[abc]"),
		"t",
	}, 2)
	trie.Set([]interface{}{
		re("[abc]"),
		"j",
	}, 3)

	_, args, matched := trie.Get([]string{
		"a",
		"t",
	})
	require.Equal(t, []interface{}{"a"}, args)
	require.Equal(t, true, matched)

	_, args2, matched := trie.Get([]string{
		"a",
		"j",
	})
	require.Equal(t, []interface{}{"a"}, args2)
	require.Equal(t, true, matched)
}
