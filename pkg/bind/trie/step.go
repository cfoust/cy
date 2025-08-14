package trie

import (
	"regexp"
)

// Step represents a single step in a sequence that can be matched against input
type Step interface {
	// Match returns true if the given input matches this step
	Match(input string) bool
	// Equal returns true if this step is equivalent to another step
	Equal(other Step) bool
}

// LiteralStep represents an exact string match
type LiteralStep struct {
	Value string
}

func NewLiteralStep(value string) *LiteralStep {
	return &LiteralStep{Value: value}
}

func (s *LiteralStep) Match(input string) bool {
	return s.Value == input
}

func (s *LiteralStep) Equal(other Step) bool {
	if otherString, ok := other.(*LiteralStep); ok {
		return s.Value == otherString.Value
	}
	return false
}

// RegexStep represents a regex pattern match
type RegexStep struct {
	Pattern  string
	compiled *regexp.Regexp
}

func NewRegexStep(pattern string) (*RegexStep, error) {
	compiled, err := regexp.Compile(pattern)
	if err != nil {
		return nil, err
	}

	return &RegexStep{
		Pattern:  pattern,
		compiled: compiled,
	}, nil
}

func (r *RegexStep) Match(input string) bool {
	return r.compiled.MatchString(input)
}

func (r *RegexStep) Equal(other Step) bool {
	if otherRegex, ok := other.(*RegexStep); ok {
		return r.Pattern == otherRegex.Pattern
	}
	return false
}

// Regex struct for backward compatibility
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

// CountStep represents a pattern that should match between min and max times
type CountStep struct {
	Pattern Step
	Min     int
	Max     int
}

func NewCountStep(pattern Step, min, max int) *CountStep {
	return &CountStep{
		Pattern: pattern,
		Min:     min,
		Max:     max,
	}
}

func (c *CountStep) Match(input string) bool {
	return c.Pattern.Match(input)
}

func (c *CountStep) Equal(other Step) bool {
	if otherCount, ok := other.(*CountStep); ok {
		return c.Min == otherCount.Min &&
			c.Max == otherCount.Max &&
			c.Pattern.Equal(otherCount.Pattern)
	}
	return false
}

// Count represents a pattern that should match between min and max times
type Count struct {
	Pattern interface{} // Can be string or *Regex
	Min     int
	Max     int
	next    interface{}
}

func NewCount(pattern interface{}, min, max int) (*Count, error) {
	count := &Count{
		Pattern: pattern,
		Min:     min,
		Max:     max,
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
