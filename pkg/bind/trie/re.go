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
