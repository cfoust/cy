package trie

import (
	"regexp"
)

type Regex struct {
	Pattern  string
	compiled *regexp.Regexp
	next     interface{}
}

func NewRegex(pattern string) (*Regex, error) {
	re := &Regex{
		Pattern: pattern,
	}

	compiled, err := regexp.Compile(pattern)
	if err != nil {
		return nil, err
	}

	re.compiled = compiled

	return re, nil
}

// Count represents a pattern that should match between min and max times
type Count struct {
	Pattern interface{} // Can be string or *Regex
	Min     int
	Max     int
	next    interface{}
	
	// For string patterns, we store the compiled regex if needed
	compiled *regexp.Regexp
}

func NewCount(pattern interface{}, min, max int) (*Count, error) {
	count := &Count{
		Pattern: pattern,
		Min:     min,
		Max:     max,
	}
	
	// If the pattern is a string, we'll treat it as an exact match
	// If it's a regex, we'll use regex matching
	if str, ok := pattern.(string); ok {
		// For string patterns, we create a regex that matches exactly that string
		compiled, err := regexp.Compile("^" + regexp.QuoteMeta(str) + "$")
		if err != nil {
			return nil, err
		}
		count.compiled = compiled
	}
	
	return count, nil
}

// Matches checks if a given string matches this count pattern
func (c *Count) Matches(s string) bool {
	switch p := c.Pattern.(type) {
	case string:
		return p == s
	case *Regex:
		return p.compiled.MatchString(s)
	default:
		return false
	}
}
