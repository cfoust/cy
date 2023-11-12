package search

import (
	"fmt"
	"regexp"
	"regexp/syntax"
	"strings"

	"github.com/cfoust/cy/pkg/geom"
)

func getFirst(re *syntax.Regexp) (string, error) {
	switch re.Op {
	case syntax.OpNoMatch, syntax.OpEmptyMatch:
		return "", fmt.Errorf("regex matches nothing")
	case syntax.OpAnyCharNotNL, syntax.OpAnyChar:
		// TODO(cfoust): 11/11/23 this should just be a warning
		return "", fmt.Errorf("regex starts with any char")
	case syntax.OpLiteral:
		return string(re.Rune[0]), nil
	case syntax.OpCharClass:
		return re.String(), nil
	case syntax.OpStar, syntax.OpPlus, syntax.OpQuest, syntax.OpRepeat:
		return re.Sub[0].String(), nil
	case syntax.OpConcat:
		return getFirst(re.Sub[0])
	case syntax.OpCapture:
		return getFirst(re.Sub[0])
	case syntax.OpAlternate:
		var subs []string
		for _, sub := range re.Sub {
			pattern, err := getFirst(sub)
			if err != nil {
				return "", err
			}
			subs = append(subs, pattern)
		}
		return fmt.Sprintf("(%s)", strings.Join(subs, "|")), nil
	default:
		return "", fmt.Errorf("initial operator not supported: %d", re.Op)
	}
}

func getPartial(pattern string) (*regexp.Regexp, error) {
	ast, err := syntax.Parse(pattern, syntax.Perl)
	if err != nil {
		return nil, err
	}

	first, err := getFirst(ast.Simplify())
	if err != nil {
		return nil, err
	}
	return regexp.Compile(first)
}

func matchCell(re *regexp.Regexp, reader *ScreenReader, cell geom.Vec2) (loc []geom.Vec2, partial bool) {
	reader.Reset(cell)
	indices := re.FindReaderIndex(reader)

	// there was a full match
	if len(indices) == 2 {
		loc = []geom.Vec2{
			reader.GetLocation(indices[0]),
			reader.GetLocation(indices[1]),
		}
		return
	}

	// FindReaderIndex will always read three runes if you prefix with ^;
	// if that's all it read, there was no partial match
	if reader.NumRead() <= 3 {
		return
	}

	loc = []geom.Vec2{
		cell,
		reader.Next(),
	}

	// If we read any more, there was a partial match no matter what
	partial = true
	return
}
