package fuzzy

import (
	"fmt"
	"strings"

	"github.com/cfoust/cy/pkg/input/fuzzy/fzf"
	"github.com/cfoust/cy/pkg/input/fuzzy/fzf/util"
	"github.com/cfoust/cy/pkg/input/fuzzy/preview"
	"github.com/cfoust/cy/pkg/janet"
)

type Match struct {
	Score int
	Index *[]int
}

type Option struct {
	Text    string
	Columns []string
	Preview any
	Chars   *util.Chars
	Match   *Match
	Result  any
}

func (o *Option) setText(text string) {
	chars := util.ToChars([]byte(text))
	o.Text = text
	o.Chars = &chars
}

type tupleInput struct {
	_ struct{} `janet:"tuple"`
	// the text that the user will filter
	Text *janet.Value
	// the value returned if the user chooses this option
	Value *janet.Value
}

type tripleInput struct {
	_ struct{} `janet:"tuple"`
	// the text that the user will filter
	Text *janet.Value
	// contains the configuration for the preview window
	Preview *janet.Value
	// the value returned if the user chooses this option
	Value *janet.Value
}

func NewOption(text string, result any) Option {
	chars := util.ToChars([]byte(text))
	return Option{
		Text:   text,
		Chars:  &chars,
		Result: result,
	}
}

func unmarshalString(input *janet.Value, option *Option) error {
	var str string
	err := input.Unmarshal(&str)
	if err != nil {
		return err
	}

	option.setText(str)
	option.Result = str
	return nil
}

// unmarshalText unmarshals the displayed text of an option, which can consist of either a string or a
// tuple/array of strings.
func unmarshalText(input *janet.Value, option *Option) error {
	err := unmarshalString(input, option)
	if err == nil {
		return nil
	}

	var columns []string
	err = input.Unmarshal(&columns)
	if err != nil {
		return err
	}

	// Join the columns to be used as a search string
	option.setText(strings.Join(columns, "|"))
	option.Columns = columns
	return nil
}

// Unmarshal a single Option from a Janet value.
func unmarshalOption(input *janet.Value) (result Option, err error) {
	err = unmarshalString(input, &result)
	if err == nil {
		return
	}

	var tuple tupleInput
	err = input.Unmarshal(&tuple)
	if err == nil {
		err = unmarshalText(tuple.Text, &result)
		if err != nil {
			return
		}
		result.Result = tuple.Value
		return
	}

	var triple tripleInput
	err = input.Unmarshal(&triple)
	if err != nil {
		err = fmt.Errorf("input must be array of strings or tuples")
		return
	}

	err = unmarshalText(triple.Text, &result)
	if err != nil {
		return
	}
	result.Result = triple.Value
	result.Preview, err = preview.Unmarshal(triple.Preview)
	return
}

func UnmarshalOptions(input *janet.Value) (result []Option, err error) {
	var values []*janet.Value
	var option Option
	err = input.Unmarshal(&values)
	numColumns := 0
	for i, value := range values {
		option, err = unmarshalOption(value)
		if err != nil {
			err = fmt.Errorf("item %d is malformed: %s", i, err.Error())
			return
		}
		if len(option.Columns) > 0 {
			numColumns = len(option.Columns)
		}
		result = append(result, option)
	}

	for i, option := range result {
		if len(option.Columns) != numColumns {
			err = fmt.Errorf(
				"row %d has invalid number of columns (has %d, need %d)",
				i,
				len(option.Columns),
				numColumns,
			)
			return
		}
	}

	return
}

func Filter(
	options []Option,
	search string,
	caseSensitive bool,
) []Option {
	matches := make([]Option, 0)
	for _, option := range options {
		result, pos := fzf.FuzzyMatchV2(
			caseSensitive,
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
