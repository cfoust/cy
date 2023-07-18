package fuzzy

import (
	"sort"

	"github.com/cfoust/cy/pkg/fuzzy/fzf"
	"github.com/cfoust/cy/pkg/fuzzy/fzf/util"
)

type Match struct {
	Text  string
	Score int
	Chars *[]int
}

func Filter(options []string, search string) []Match {
	matches := make([]Match, 0)
	for _, option := range options {
		input := util.ToChars([]byte(option))
		result, pos := fzf.FuzzyMatchV2(
			true,
			true,
			true,
			&input,
			[]rune(search),
			true,
			nil,
		)

		if result.Score == 0 {
			continue
		}

		matches = append(matches, Match{
			Text:  option,
			Score: result.Score,
			Chars: pos,
		})
	}

	sort.Slice(matches, func(i, j int) bool {
		return matches[i].Score > matches[j].Score
	})

	return matches
}
