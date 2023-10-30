package fuzzy

import (
	"fmt"

	"github.com/cfoust/cy/pkg/fuzzy/fzf"
	"github.com/cfoust/cy/pkg/fuzzy/fzf/util"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
)

type Match struct {
	Score int
	Index *[]int
}

type Option struct {
	Text string
	// Supported types:
	// - string: will be passed to a blank terminal
	// - NodeID: will show in the background
	Preview interface{}
	Chars   *util.Chars
	Match   *Match
	Result  interface{}
}

type tupleInput struct {
	_     struct{} `janet:"tuple"`
	Text  string
	Value *janet.Value
}

type tripleInput struct {
	_       struct{} `janet:"tuple"`
	Text    string
	Preview *janet.Value
	Value   *janet.Value
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

	var triples []tripleInput
	var previewNode tree.NodeID
	var previewStr string
	err = input.Unmarshal(&triples)
	if err == nil {
		for i, triple := range triples {
			option := createOption(
				triple.Text,
				triple.Value,
			)

			nodeErr := triple.Preview.Unmarshal(&previewNode)
			strErr := triple.Preview.Unmarshal(&previewStr)
			if nodeErr != nil && strErr != nil {
				err = fmt.Errorf("preview for choice %d had invalid type", i)
				return
			}

			if nodeErr == nil {
				option.Preview = previewNode
			}

			if strErr == nil {
				option.Preview = previewStr
			}

			result = append(result, option)
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
