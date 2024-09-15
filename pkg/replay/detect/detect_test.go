package detect

import (
	"testing"
	"time"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	P "github.com/cfoust/cy/pkg/io/protocol"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/sessions/search"

	"github.com/stretchr/testify/require"
)

func promptTest(
	t *testing.T,
	commands []Command,
	setup ...interface{},
) {
	s := sessions.NewSimulator().
		Defaults()

	for _, event := range setup {
		s.AddTime(time.Second, event)
	}

	events := s.Events()

	reported := make([]Command, 0)
	handler := func(c Command) {
		reported = append(reported, c)
	}

	d := New(WithHandler(handler))
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

	// This seems silly, but ExecutedAt and CompletedAt are straightforward
	// copies of the time stamps at each index and not worth testing
	// individually
	var zero time.Time
	for i, command := range commands {
		commands[i].ExecutedAt = zero.Add(time.Duration(command.Executed-1) * time.Second)
		commands[i].CompletedAt = zero.Add(time.Duration(command.Completed-1) * time.Second)
	}

	require.True(t, d.havePrompt)
	require.Equal(t, commands, d.Commands(term, events))

	if d.commands != nil {
		require.Equal(t, d.commands, reported)
	}
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
		TEST_PROMPT, "command\n",
		"foo\n",
		"bar\n",
		"baz\n",
		TEST_PROMPT,
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
		TEST_PROMPT, "command\n",
		"foo\n",
		TEST_PROMPT, "command\n",
		"foo\n",
		TEST_PROMPT,
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
		TEST_PROMPT, "command\n",
		"foo\n",
		"bar\n",
		"baz", TEST_PROMPT,
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
		TEST_PROMPT, "command\n",
		TEST_PROMPT,
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
		TEST_PROMPT, "command\n",
		"> ", "foo\n",
		"> ", "\n",
		"> ", "baz\n",
		"output\n",
		TEST_PROMPT,
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
		TEST_PROMPT, "command\n",
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
		TEST_PROMPT, "command\n",
		"> ", "foo\n",
		"> ", "\n",
		"> ", "baz\n",
		"output\n",
	)
}

func TestDirectory(t *testing.T) {
	promptSingle(
		t,
		Command{
			Directory: "test",
			Text:      "command",
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
			promptedWrite: 3,
			Prompted:      3,
			Executed:      4,
			Completed:     7,
		},
		// set the directory with OSC-7
		"\033]7;test\033\\",
		TEST_PROMPT, "command\n",
		"foo\n",
		"bar\n",
		"baz\n",
		TEST_PROMPT,
	)
}

func TestDirectoryChange(t *testing.T) {
	promptTest(
		t,
		[]Command{
			{
				Directory: "test",
				Text:      "command",
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
				promptedWrite: 3,
				Prompted:      3,
				Executed:      4,
				Completed:     5,
			},
			{
				// still test, since dir didn't change until
				// after command executed
				Directory: "test",
				Text:      "command",
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
				promptedWrite: 6,
				Prompted:      6,
				Executed:      7,
				Completed:     9,
			},
			{
				Directory: "test2",
				Text:      "command",
				Input: []search.Selection{
					{
						From: geom.Vec2{R: 4, C: 2},
						To:   geom.Vec2{R: 4, C: 9},
					},
				},
				Output: search.Selection{
					From: geom.Vec2{R: 5, C: 0},
					To:   geom.Vec2{R: 5, C: 3},
				},
				promptedWrite: 10,
				Prompted:      10,
				Executed:      11,
				Completed:     12,
			},
		},
		// set dir before first
		"\033]7;test\033\\",
		TEST_PROMPT, "command\n",
		"foo\n",
		TEST_PROMPT, "command\n",
		// set dir during the second
		"\033]7;test2\033\\",
		"foo\n",
		// occurred in test2
		TEST_PROMPT, "command\n",
		"foo\n",
		TEST_PROMPT,
	)
}
