package motion

import (
	"regexp"

	"github.com/cfoust/cy/pkg/geom"
)

var (
	// vim word
	// warning: not accurate
	WORD_REGEX = regexp.MustCompile(`[!-~\w]+`)
	// vim WORD
	NON_WHITESPACE_REGEX = regexp.MustCompile(`[^\s]+`)
)

type indexFunc func(match []int) int

var (
	getBeginning indexFunc = func(match []int) int {
		return match[0]
	}
	getEnd indexFunc = func(match []int) int {
		return match[1] - 1
	}
)

func getLineMatches(
	m Movable,
	re *regexp.Regexp,
	row int,
) (loc [][]int, ok bool) {
	line, ok := m.Line(row)
	if !ok {
		return
	}

	return findAllLine(re, line), true
}

func nextWord(
	m Movable,
	re *regexp.Regexp,
	index indexFunc,
	isForward bool,
) (
	dest geom.Vec2,
	ok bool,
) {
	cursor := m.Cursor()

	matches, matchOk := getLineMatches(m, re, cursor.R)
	if !matchOk {
		return
	}

	if isForward {
		for _, match := range matches {
			if index(match) <= cursor.C {
				continue
			}

			return geom.Vec2{
				R: cursor.R,
				C: index(match),
			}, true
		}
	} else {
		for i := len(matches) - 1; i >= 0; i-- {
			match := matches[i]
			if index(match) >= cursor.C {
				continue
			}

			return geom.Vec2{
				R: cursor.R,
				C: index(match),
			}, true
		}
	}

	// If we're here, there was no match on this line
	row := cursor.R
	for {
		if isForward {
			row++
		} else {
			row--
		}

		line, lineOk := m.Line(row)
		if !lineOk {
			return
		}

		// Blank lines count as words
		if len(line) == 0 {
			return geom.Vec2{
				R: row,
				C: 0,
			}, true
		}

		matches := findAllLine(re, line)

		// Non-blank lines with no words are skipped
		if len(matches) == 0 {
			continue
		}

		match := matches[0]
		if !isForward {
			match = matches[len(matches)-1]
		}

		return geom.Vec2{
			R: row,
			C: index(match),
		}, true
	}
}

func Word(m Movable, isForward, isEnd bool) {
	index := getBeginning
	if isEnd {
		index = getEnd
	}

	dest, ok := nextWord(m, WORD_REGEX, index, isForward)
	if !ok {
		return
	}

	m.Goto(dest)
}

func WORD(m Movable, isForward, isEnd bool) {
	index := getBeginning
	if isEnd {
		index = getEnd
	}

	dest, ok := nextWord(m, NON_WHITESPACE_REGEX, index, isForward)
	if !ok {
		return
	}

	m.Goto(dest)
}
