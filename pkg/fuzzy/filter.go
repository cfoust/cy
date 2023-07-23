package fuzzy

import (
	"fmt"

	"github.com/cfoust/cy/pkg/fuzzy/fzf"
	"github.com/cfoust/cy/pkg/fuzzy/fzf/util"
	"github.com/cfoust/cy/pkg/janet"
)

type Match struct {
	Score int
	Index *[]int
}

type Option struct {
	Text   string
	Chars  *util.Chars
	Match  *Match
	Result interface{}
}

type tupleInput struct {
	_     struct{} `janet:"tuple"`
	Text  string
	Value *janet.Value
}

func createOption(text string, result interface{}) Option {
	chars := util.ToChars([]byte(text))
	return Option{
		Text:   text,
		Chars:  &chars,
		Result: result,
	}
}

func UnmarshalOptions(input *janet.Value) (result []Option, err error) {
	var strings []string
	err = input.Unmarshal(&strings)
	if err == nil {
		for _, str := range strings {
			result = append(
				result,
				createOption(str, str),
			)
		}

		return
	}

	var tuples []tupleInput
	err = input.Unmarshal(&tuples)
	if err == nil {
		for _, tuple := range tuples {
			result = append(
				result,
				createOption(
					tuple.Text,
					tuple.Value,
				),
			)
		}
		return
	}

	err = fmt.Errorf("input must be array of strings or tuples")
	return
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
