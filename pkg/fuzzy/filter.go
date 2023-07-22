package fuzzy

import (
	"github.com/cfoust/cy/pkg/fuzzy/fzf"
	"github.com/cfoust/cy/pkg/fuzzy/fzf/util"
)

type Match struct {
	Score int
	Index *[]int
}

type Option struct {
	Text  string
	Chars *util.Chars
	Match *Match
}

func createOptions(options []string) (result []Option) {
	for _, option := range options {
		chars := util.ToChars([]byte(option))
		result = append(result, Option{
			Text:  option,
			Chars: &chars,
		})
	}

	return result
}

func Filter(options []Option, search string) []Option {
	matches := make([]Option, 0)
	for _, option := range options {
		result, pos := fzf.FuzzyMatchV2(
			true,
			true,
			true,
			option.Chars,
			[]rune(search),
			true,
			nil,
		)

		if result.Score == 0 {
			continue
		}

		newOption := option
		newOption.Match = &Match{
			Score: result.Score,
			Index: pos,
		}
		matches = append(matches, newOption)
	}

	return matches
}
