package trie

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func re(pattern string) *RegexStep {
	r, err := NewRegexStep(pattern)
	if err != nil {
		panic(err)
	}

	return r
}

func str(pattern string) *LiteralStep {
	return NewLiteralStep(pattern)
}

func count(step Step, min, max int) *CountStep {
	return NewCountStep(step, min, max)
}

func TestTrie(t *testing.T) {
	trie := New[int]()
	trie.Set([]Step{
		str("one"),
		str("two"),
	}, 2)

	_, _, matched := trie.Get([]string{
		"one",
		"two",
	})
	require.Equal(t, true, matched)

	trie.Set([]Step{
		str("one"),
		str("three"),
	}, 1)

	trie.Set([]Step{
		str("two"),
	}, 1)

	// The tree:
	// one -> two -> 2
	// one -> three -> 1
	// two -> 1
	require.Equal(t, 3, len(trie.Leaves()))
	require.Equal(t, 2, len(trie.Partial([]string{
		"one",
	})))

	trie.Remap([]Step{
		str("one"),
	}, []Step{
		str("three"),
	})
	// The tree:
	// three -> two -> 2
	// three -> three -> 1
	// two -> 1
	require.Equal(t, 3, len(trie.Leaves()))
	require.Equal(t, 2, len(trie.Partial([]string{
		"three",
	})))

	trie.Clear([]Step{
		str("three"),
	})
	// The tree:
	// two -> 1
	require.Equal(t, 1, len(trie.Leaves()))
	require.Equal(t, 0, len(trie.Partial([]string{
		"one",
	})))

	trie.Clear([]Step{})
	require.Equal(t, 0, len(trie.Leaves()))
}

func TestManyRemap(t *testing.T) {
	trie := New[int]()

	trie.Set([]Step{
		str("one"),
		str("three"),
	}, 1)

	trie.Remap([]Step{
		str("one"),
	}, []Step{
		str("three"),
	})

	require.Equal(t, 1, len(trie.Leaves()))

	trie.Remap([]Step{
		str("one"),
	}, []Step{
		str("three"),
	})
	require.Equal(t, 1, len(trie.Leaves()))
}

func TestRegex(t *testing.T) {
	trie := New[int]()
	trie.Set([]Step{
		re("[abc]"),
		str("t"),
	}, 2)

	_, args, ok := trie.Get([]string{
		"a",
		"t",
	})
	require.Equal(t, [][]string{{"a"}, {"t"}}, args)
	require.True(t, ok)

	_, _, ok = trie.Get([]string{
		"d",
		"t",
	})
	require.False(t, ok)
}

func TestMultipleRegex(t *testing.T) {
	trie := New[int]()
	trie.Set([]Step{
		re("[abc]"),
		str("t"),
	}, 2)
	trie.Set([]Step{
		re("[abc]"),
		str("j"),
	}, 3)

	_, args, ok := trie.Get([]string{
		"a",
		"t",
	})
	require.Equal(t, [][]string{{"a"}, {"t"}}, args)
	require.True(t, ok)

	_, args2, ok := trie.Get([]string{
		"a",
		"j",
	})
	require.Equal(t, [][]string{{"a"}, {"j"}}, args2)
	require.True(t, ok)
}
