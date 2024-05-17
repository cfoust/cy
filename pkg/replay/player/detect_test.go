package player

import (
	"testing"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
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
	writes := []interface{}{
		geom.DEFAULT_SIZE,
		emu.LineFeedMode,
	}
	writes = append(writes, setup...)

	events := sessions.NewSimulator().
		Add(writes...).
		Events()

	p := FromEvents(events)
	d := p.detector
	require.True(t, d.havePrompt)
	require.Equal(t, commands, d.Commands())
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
			Pending: true,
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
			Pending: true,
		},
		PROMPT, "command\n",
		"> ", "foo\n",
		"> ", "\n",
		"> ", "baz\n",
		"output\n",
	)
}
