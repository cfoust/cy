package detect

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/sessions/search"

	"github.com/stretchr/testify/require"
)

const (
	PROMPT = "\033Pcy\033\\$ "
)

func promptTest(
	t *testing.T,
	commands []Command,
	setup ...interface{},
) {
	events := sessions.NewSimulator().
		Defaults().
		Add(setup...).
		Events()

	d := New()
	term := emu.New()
	term.Changes().SetHooks([]string{CY_HOOK})
	for i := 0; i < len(events); i++ {
		event := events[i]
		switch e := event.Message.(type) {
		case P.OutputMessage:
			term.Parse(e.Data)
			d.Detect(term, events[0:i+1])
		case P.SizeMessage:
			term.Resize(e.Vec())
		}
	}

	require.True(t, d.havePrompt)
	require.Equal(t, commands, d.Commands(term, events))
}

func promptSingle(
	t *testing.T,
	command Command,
	setup ...interface{},
) {
	promptTest(t, []Command{command}, setup...)
}

func TestSimple(t *testing.T) {
	promptSingle(
		t,
		Command{
			Text: "command",
			Input: []search.Selection{
				{
					From: geom.Vec2{R: 0, C: 2},
					To:   geom.Vec2{R: 0, C: 9},
				},
			},
			Output: search.Selection{
				From: geom.Vec2{R: 1, C: 0},
				To:   geom.Vec2{R: 3, C: 3},
			},
			promptedWrite: 2,
			Prompted:      2,
			Executed:      3,
			Completed:     6,
		},
		PROMPT, "command\n",
		"foo\n",
		"bar\n",
		"baz\n",
		PROMPT,
	)
}

func TestTwo(t *testing.T) {
	promptTest(
		t,
		[]Command{
			{
				Text: "command",
				Input: []search.Selection{
					{
						From: geom.Vec2{R: 0, C: 2},
						To:   geom.Vec2{R: 0, C: 9},
					},
				},
				Output: search.Selection{
					From: geom.Vec2{R: 1, C: 0},
					To:   geom.Vec2{R: 1, C: 3},
				},
				promptedWrite: 2,
				Prompted:      2,
				Executed:      3,
				Completed:     4,
			},
			{
				Text: "command",
				Input: []search.Selection{
					{
						From: geom.Vec2{R: 2, C: 2},
						To:   geom.Vec2{R: 2, C: 9},
					},
				},
				Output: search.Selection{
					From: geom.Vec2{R: 3, C: 0},
					To:   geom.Vec2{R: 3, C: 3},
				},
				promptedWrite: 5,
				Prompted:      5,
				Executed:      6,
				Completed:     7,
			},
		},
		PROMPT, "command\n",
		"foo\n",
		PROMPT, "command\n",
		"foo\n",
		PROMPT,
	)
}

// Sometimes the output does not have a final \n, meaning that the prompt is
// printed on the last line of the output.
func TestEndSameLine(t *testing.T) {
	promptSingle(
		t,
		Command{
			Text: "command",
			Input: []search.Selection{
				{
					From: geom.Vec2{R: 0, C: 2},
					To:   geom.Vec2{R: 0, C: 9},
				},
			},
			Output: search.Selection{
				From: geom.Vec2{R: 1, C: 0},
				To:   geom.Vec2{R: 3, C: 3},
			},
			promptedWrite: 2,
			Prompted:      2,
			Executed:      3,
			Completed:     6,
		},
		PROMPT, "command\n",
		"foo\n",
		"bar\n",
		"baz", PROMPT,
	)
}

func TestNoOutput(t *testing.T) {
	promptSingle(
		t,
		Command{
			Text: "command",
			Input: []search.Selection{
				{
					From: geom.Vec2{R: 0, C: 2},
					To:   geom.Vec2{R: 0, C: 9},
				},
			},
			Output: search.Selection{
				From: geom.Vec2{R: 1, C: 0},
				To:   geom.Vec2{R: 1, C: 0},
			},
			promptedWrite: 2,
			Prompted:      2,
			Executed:      3,
			Completed:     3,
		},
		PROMPT, "command\n",
		PROMPT,
	)
}

func TestMulti(t *testing.T) {
	promptSingle(
		t,
		Command{
			Text: "command\nfoo\n\nbaz",
			Input: []search.Selection{
				{
					From: geom.Vec2{R: 0, C: 2},
					To:   geom.Vec2{R: 0, C: 9},
				},
				{
					From: geom.Vec2{R: 1, C: 2},
					To:   geom.Vec2{R: 1, C: 5},
				},
				{
					From: geom.Vec2{R: 2, C: 2},
					To:   geom.Vec2{R: 2, C: 2},
				},
				{
					From: geom.Vec2{R: 3, C: 2},
					To:   geom.Vec2{R: 3, C: 5},
				},
			},
			Output: search.Selection{
				From: geom.Vec2{R: 4, C: 0},
				To:   geom.Vec2{R: 4, C: 6},
			},
			promptedWrite: 2,
			Prompted:      2,
			Executed:      9,
			Completed:     10,
		},
		PROMPT, "command\n",
		"> ", "foo\n",
		"> ", "\n",
		"> ", "baz\n",
		"output\n",
		PROMPT,
	)
}

func TestPending(t *testing.T) {
	promptSingle(
		t,
		Command{
			Text: "command",
			Input: []search.Selection{
				{
					From: geom.Vec2{R: 0, C: 2},
					To:   geom.Vec2{R: 0, C: 9},
				},
			},
			Output: search.Selection{
				From: geom.Vec2{R: 1, C: 0},
				To:   geom.Vec2{R: 1, C: 3},
			},
			Pending:       true,
			promptedWrite: 2,
			Prompted:      2,
			Executed:      3,
			Completed:     4,
		},
		PROMPT, "command\n",
		"foo",
	)
}

func TestPendingMulti(t *testing.T) {
	promptSingle(
		t,
		Command{
			Text: "command\nfoo\n\nbaz",
			Input: []search.Selection{
				{
					From: geom.Vec2{R: 0, C: 2},
					To:   geom.Vec2{R: 0, C: 9},
				},
				{
					From: geom.Vec2{R: 1, C: 2},
					To:   geom.Vec2{R: 1, C: 5},
				},
				{
					From: geom.Vec2{R: 2, C: 2},
					To:   geom.Vec2{R: 2, C: 2},
				},
				{
					From: geom.Vec2{R: 3, C: 2},
					To:   geom.Vec2{R: 3, C: 5},
				},
			},
			Output: search.Selection{
				From: geom.Vec2{R: 4, C: 0},
				To:   geom.Vec2{R: 4, C: 6},
			},
			Pending:       true,
			promptedWrite: 2,
			Prompted:      2,
			Executed:      9,
			Completed:     10,
		},
		PROMPT, "command\n",
		"> ", "foo\n",
		"> ", "\n",
		"> ", "baz\n",
		"output\n",
	)
}
