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

func TestStepBasedTrie(t *testing.T) {
	trie := New[int](nil)
	
	// Test basic string steps
	trie.Set([]interface{}{
		"one",
		"two",
	}, 2)

	value, args, matched := trie.Get([]string{
		"one",
		"two",
	})
	require.Equal(t, true, matched)
	require.Equal(t, 2, value)
	require.Equal(t, 0, len(args))

	// Test regex steps
	trie.Set([]interface{}{
		re("[abc]"),
		"test",
	}, 5)

	value2, args2, matched2 := trie.Get([]string{
		"a",
		"test",
	})
	require.Equal(t, true, matched2)
	require.Equal(t, 5, value2)
	require.Equal(t, []interface{}{"a"}, args2)

	// Test no match
	_, _, matched3 := trie.Get([]string{
		"d",
		"test",
	})
	require.Equal(t, false, matched3)
}

func count(pattern interface{}, min, max int) *Count {
	c, err := NewCount(pattern, min, max)
	if err != nil {
		panic(err)
	}
	return c
}

func TestStepBasedTrieWithCount(t *testing.T) {
	trie := New[string](nil)
	
	// Create a count pattern: (key/count "a" 1 3)
	countPattern := count("a", 1, 3)
	
	// Set a binding: ["f", count, "g"] -> "test"
	sequence := []interface{}{"f", countPattern, "g"}
	trie.Set(sequence, "test")
	
	// Test that it matches different repetitions using the new step-based approach
	testCases := []struct {
		input    []string
		expected string
		should   bool
	}{
		{[]string{"f", "a", "g"}, "test", true},           // 1 repetition
		{[]string{"f", "a", "a", "g"}, "test", true},      // 2 repetitions  
		{[]string{"f", "a", "a", "a", "g"}, "test", true}, // 3 repetitions
		{[]string{"f", "g"}, "", false},                   // 0 repetitions (should not match)
		{[]string{"f", "a", "a", "a", "a", "g"}, "", false}, // 4 repetitions (should not match)
	}
	
	for _, tc := range testCases {
		value, args, matched := trie.Get(tc.input)
		if matched != tc.should {
			t.Errorf("Input %v: expected matched=%v, got %v", tc.input, tc.should, matched)
		}
		if matched && value != tc.expected {
			t.Errorf("Input %v: expected value='%s', got '%s'", tc.input, tc.expected, value)
		}
		if matched && tc.should {
			// Count patterns should return args
			require.Equal(t, 1, len(args), "Expected 1 arg for count pattern")
		}
	}
}
