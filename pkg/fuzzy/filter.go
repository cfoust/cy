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
	_ struct{} `janet:"tuple"`
	// the text that the user will filter
	Text string
	// the value returned if the user chooses this option
	Value *janet.Value
}

type tripleInput struct {
	_ struct{} `janet:"tuple"`
	// the text that the user will filter
	Text string
	// contains the configuration for the preview window
	Preview *janet.Value
	// the value returned if the user chooses this option
	Value *janet.Value
}

var (
	KEYWORD_TEXT   = janet.Keyword("text")
	KEYWORD_NODE   = janet.Keyword("node")
	KEYWORD_REPLAY = janet.Keyword("replay")
)

type previewInput struct {
	_     struct{} `janet:"tuple"`
	Type  janet.Keyword
	Value *janet.Value
}

type textPreview struct {
	_    struct{} `janet:"tuple"`
	Text string
}

type nodePreview struct {
	_  struct{} `janet:"tuple"`
	Id tree.NodeID
}

type replayPreview struct {
	_    struct{} `janet:"tuple"`
	Path string
}

func NewOption(text string, result interface{}) Option {
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
				NewOption(str, str),
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
				NewOption(
					tuple.Text,
					tuple.Value,
				),
			)
		}
		return
	}

	var triples []tripleInput
	err = input.Unmarshal(&triples)
	if err != nil {
		err = fmt.Errorf("input must be array of strings or tuples")
		return
	}

	for i, triple := range triples {
		option := NewOption(
			triple.Text,
			triple.Value,
		)

		preview := previewInput{}
		preview.Type = KEYWORD_TEXT
		err = triple.Preview.Unmarshal(&preview)
		if err == nil {
			text := textPreview{}
			err = preview.Value.Unmarshal(&text)
			if err != nil {
				return
			}
			option.Preview = text
			result = append(result, option)
			continue
		}

		preview.Type = KEYWORD_NODE
		err = triple.Preview.Unmarshal(&preview)
		if err == nil {
			node := nodePreview{}
			err = preview.Value.Unmarshal(&node)
			if err != nil {
				return
			}
			option.Preview = node
			result = append(result, option)
			continue
		}

		preview.Type = KEYWORD_REPLAY
		err = triple.Preview.Unmarshal(&preview)
		if err == nil {
			replay := replayPreview{}
			err = preview.Value.Unmarshal(&replay)
			if err != nil {
				return
			}
			option.Preview = replay
			result = append(result, option)
			continue
		}

		err = fmt.Errorf("invalid preview for option %d", i)
		return
	}

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
