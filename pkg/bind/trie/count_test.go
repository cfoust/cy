package trie

import (
	"testing"
)

func TestKeyRe(t *testing.T) {
	// Test creating a regex pattern
	re, err := NewRegex("[0-9]")
	if err != nil {
		t.Fatalf("Failed to create regex: %v", err)
	}
	
	if re.Pattern != "[0-9]" {
		t.Errorf("Expected pattern '[0-9]', got '%s'", re.Pattern)
	}
	
	// Test that it matches digits
	if !re.compiled.MatchString("5") {
		t.Error("Regex should match '5'")
	}
	
	if re.compiled.MatchString("a") {
		t.Error("Regex should not match 'a'")
	}
}

func TestKeyCount(t *testing.T) {
	// Test creating a count pattern with string
	count, err := NewCount("a", 1, 3)
	if err != nil {
		t.Fatalf("Failed to create count: %v", err)
	}
	
	if count.Min != 1 || count.Max != 3 {
		t.Errorf("Expected min=1, max=3, got min=%d, max=%d", count.Min, count.Max)
	}
	
	// Test matches
	if !count.Matches("a") {
		t.Error("Count should match 'a'")
	}
	
	if count.Matches("b") {
		t.Error("Count should not match 'b'")
	}
}

func TestKeyCountWithRegex(t *testing.T) {
	// Test creating a count pattern with regex
	re, err := NewRegex("[0-9]")
	if err != nil {
		t.Fatalf("Failed to create regex: %v", err)
	}
	
	count, err := NewCount(re, 1, 3)
	if err != nil {
		t.Fatalf("Failed to create count: %v", err)
	}
	
	// Test matches
	if !count.Matches("5") {
		t.Error("Count should match '5'")
	}
	
	if count.Matches("a") {
		t.Error("Count should not match 'a'")
	}
}

func TestTrieWithCount(t *testing.T) {
	trie := New[string](nil)
	
	// Create a count pattern: (key/count "a" 1 3)
	count, err := NewCount("a", 1, 3)
	if err != nil {
		t.Fatalf("Failed to create count: %v", err)
	}
	
	// Set a binding: ["f", count, "g"] -> "test"
	sequence := []interface{}{"f", count, "g"}
	trie.Set(sequence, "test")
	
	// Test that it matches different repetitions
	testCases := []struct {
		input       []string
		expected    string
		should      bool
		expectedArg []string // What the count pattern should capture
	}{
		{[]string{"f", "a", "g"}, "test", true, []string{"a"}},          // 1 repetition
		{[]string{"f", "a", "a", "g"}, "test", true, []string{"a", "a"}}, // 2 repetitions
		{[]string{"f", "a", "a", "a", "g"}, "test", true, []string{"a", "a", "a"}}, // 3 repetitions
		{[]string{"f", "g"}, "", false, nil},                            // 0 repetitions (should not match)
		{[]string{"f", "a", "a", "a", "a", "g"}, "", false, nil},        // 4 repetitions (should not match)
	}
	
	for _, tc := range testCases {
		value, args, matched := trie.Get(tc.input)
		if matched != tc.should {
			t.Errorf("Input %v: expected matched=%v, got %v", tc.input, tc.should, matched)
		}
		if matched && value != tc.expected {
			t.Errorf("Input %v: expected value='%s', got '%s'", tc.input, tc.expected, value)
		}
		if matched && tc.expectedArg != nil {
			if len(args) != 1 {
				t.Errorf("Input %v: expected 1 arg, got %d", tc.input, len(args))
			} else {
				argArray, ok := args[0].([]string)
				if !ok {
					t.Errorf("Input %v: expected args[0] to be []string, got %T", tc.input, args[0])
				} else {
					if len(argArray) != len(tc.expectedArg) {
						t.Errorf("Input %v: expected arg length %d, got %d", tc.input, len(tc.expectedArg), len(argArray))
					} else {
						for i, expected := range tc.expectedArg {
							if argArray[i] != expected {
								t.Errorf("Input %v: expected arg[%d]='%s', got '%s'", tc.input, i, expected, argArray[i])
							}
						}
					}
				}
			}
		}
	}
}

func TestTrieWithRegexAndCount(t *testing.T) {
	trie := New[string](nil)
	
	// Create a regex pattern for digits
	re, err := NewRegex("[0-9]")
	if err != nil {
		t.Fatalf("Failed to create regex: %v", err)
	}
	
	// Create a count pattern: (key/count (key/re "[0-9]") 1 3)
	count, err := NewCount(re, 1, 3)
	if err != nil {
		t.Fatalf("Failed to create count: %v", err)
	}
	
	// Set a binding: [count, "g"] -> "test"
	sequence := []interface{}{count, "g"}
	trie.Set(sequence, "test")
	
	// Test that it matches different digit sequences
	testCases := []struct {
		input       []string
		expected    string
		should      bool
		expectedArg []string
	}{
		{[]string{"5", "g"}, "test", true, []string{"5"}},          // 1 digit
		{[]string{"1", "2", "g"}, "test", true, []string{"1", "2"}}, // 2 digits
		{[]string{"7", "8", "9", "g"}, "test", true, []string{"7", "8", "9"}}, // 3 digits
		{[]string{"g"}, "", false, nil},                             // 0 digits (should not match)
		{[]string{"1", "2", "3", "4", "g"}, "", false, nil},         // 4 digits (should not match)
		{[]string{"a", "g"}, "", false, nil},                        // non-digit (should not match)
	}
	
	for _, tc := range testCases {
		value, args, matched := trie.Get(tc.input)
		if matched != tc.should {
			t.Errorf("Input %v: expected matched=%v, got %v", tc.input, tc.should, matched)
		}
		if matched && value != tc.expected {
			t.Errorf("Input %v: expected value='%s', got '%s'", tc.input, tc.expected, value)
		}
		if matched && tc.expectedArg != nil {
			if len(args) != 1 {
				t.Errorf("Input %v: expected 1 arg, got %d", tc.input, len(args))
			} else {
				argArray, ok := args[0].([]string)
				if !ok {
					t.Errorf("Input %v: expected args[0] to be []string, got %T", tc.input, args[0])
				} else {
					if len(argArray) != len(tc.expectedArg) {
						t.Errorf("Input %v: expected arg length %d, got %d", tc.input, len(tc.expectedArg), len(argArray))
					} else {
						for i, expected := range tc.expectedArg {
							if argArray[i] != expected {
								t.Errorf("Input %v: expected arg[%d]='%s', got '%s'", tc.input, i, expected, argArray[i])
							}
						}
					}
				}
			}
		}
	}
}