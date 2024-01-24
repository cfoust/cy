package fuzzy

import (
	"fmt"
	"strings"

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
	Text    string
	Columns []string
	// Supported types:
	// - string: will be passed to a blank terminal
	// - NodeID: will show in the background
	Preview interface{}
	Chars   *util.Chars
	Match   *Match
	Result  interface{}
}

type tableInput struct {
	_       struct{} `janet:"tuple"`
	Columns []string
	Value   *janet.Value
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

func unmarshalOption(input *janet.Value) (result Option, err error) {
	var str string
	err = input.Unmarshal(&str)
	if err == nil {
		result = NewOption(str, str)
		return
	}

	var tuple tupleInput
	err = input.Unmarshal(&tuple)
	if err == nil {
		result = NewOption(
			tuple.Text,
			tuple.Value,
		)
		return
	}

	var triple tripleInput
	err = input.Unmarshal(&triple)
	if err != nil {
		err = fmt.Errorf("input must be array of strings or tuples")
		return
	}

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
		result = option
		return
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
		result = option
		return
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
		result = option
		return
	}

	err = fmt.Errorf("invalid preview")
	return
}

func UnmarshalOptions(input *janet.Value) (result []Option, err error) {
	var rows []tableInput
	err = input.Unmarshal(&rows)
	if err == nil {
		for i, row := range rows {
			option, parseErr := unmarshalOption(row.Value)
			if parseErr != nil {
				err = fmt.Errorf("row %d is malformed: %s", i, parseErr.Error())
				return
			}

			// Join the columns to be used as a search string
			columnOption := NewOption(strings.Join(row.Columns, "|"), nil)
			option.Columns = row.Columns
			option.Text = columnOption.Text
			option.Chars = columnOption.Chars

			result = append(result, option)
		}

		return
	}

	var values []*janet.Value
	var option Option
	err = input.Unmarshal(&values)
	for i, value := range values {
		option, err = unmarshalOption(value)
		if err != nil {
			err = fmt.Errorf("item %d is malformed: %s", i, err.Error())
			return
		}
		result = append(result, option)
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
