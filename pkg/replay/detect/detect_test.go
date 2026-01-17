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

// detectTest is a test helper for command detection tests
func detectTest(
	t *testing.T,
	commands []Command,
	options []Option,
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

	options = append(options, WithHandler(handler))

	d := New(options...)
	term := emu.New()
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

	var zero time.Time
	for i, command := range commands {
		commands[i].ExecutedAt = zero.Add(
			time.Duration(command.Executed-1) * time.Second,
		)
		commands[i].CompletedAt = zero.Add(
			time.Duration(command.Completed-1) * time.Second,
		)
	}

	require.True(t, d.havePrompt)
	require.Equal(t, commands, d.Commands(term, events))

	if d.commands != nil {
		require.Equal(t, d.commands, reported)
	}
}

// TestSimple tests basic command detection.
func TestSimple(t *testing.T) {
	detectTest(
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
					To:   geom.Vec2{R: 3, C: 3},
				},
				HasExitCode:   true,
				ExitCode:      0,
				promptedWrite: 2,
				Prompted:      2,
				Executed:      3,
				Completed:     6,
			},
		},
		[]Option{},
		// Prompt with A + "$ " + B
		emu.OSC133Prompt,
		// User types command + C marker + newline (bundled)
		"command" + emu.OSC133CommandExec + "\n",
		"foo\n",
		"bar\n",
		// Last output + D marker (bundled)
		"baz\n" + emu.OSC133CommandDone,
		// Next prompt
		emu.OSC133Prompt,
	)
}

func TestExitCodeNonZero(t *testing.T) {
	detectTest(
		t,
		[]Command{
			{
				Text: "failing-cmd",
				Input: []search.Selection{
					{
						From: geom.Vec2{R: 0, C: 2},
						To:   geom.Vec2{R: 0, C: 13},
					},
				},
				Output: search.Selection{
					From: geom.Vec2{R: 1, C: 0},
					To:   geom.Vec2{R: 1, C: 5},
				},
				HasExitCode:   true,
				ExitCode:      1,
				promptedWrite: 2,
				Prompted:      2,
				Executed:      3,
				Completed:     4,
			},
		},
		[]Option{},
		emu.OSC133Prompt,
		"failing-cmd" + emu.OSC133CommandExec + "\n",
		"error\n" + emu.OSC133CommandDone1,
		emu.OSC133Prompt,
	)
}

func TestNoExitCode(t *testing.T) {
	detectTest(
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
				HasExitCode:   false,
				ExitCode:      0,
				promptedWrite: 2,
				Prompted:      2,
				Executed:      3,
				Completed:     4,
			},
		},
		[]Option{},
		emu.OSC133Prompt,
		"command" + emu.OSC133CommandExec + "\n",
		"foo\n" + emu.OSC133CommandDoneNoEC,
		emu.OSC133Prompt,
	)
}

func TestTwoCommands(t *testing.T) {
	detectTest(
		t,
		[]Command{
			{
				Text: "first",
				Input: []search.Selection{
					{
						From: geom.Vec2{R: 0, C: 2},
						To:   geom.Vec2{R: 0, C: 7},
					},
				},
				Output: search.Selection{
					From: geom.Vec2{R: 1, C: 0},
					To:   geom.Vec2{R: 1, C: 3},
				},
				HasExitCode:   true,
				ExitCode:      0,
				promptedWrite: 2,
				Prompted:      2,
				Executed:      3,
				Completed:     4,
			},
			{
				Text: "second",
				Input: []search.Selection{
					{
						From: geom.Vec2{R: 2, C: 2},
						To:   geom.Vec2{R: 2, C: 8},
					},
				},
				Output: search.Selection{
					From: geom.Vec2{R: 3, C: 0},
					To:   geom.Vec2{R: 3, C: 3},
				},
				HasExitCode:   true,
				ExitCode:      1,
				promptedWrite: 5,
				Prompted:      5,
				Executed:      6,
				Completed:     7,
			},
		},
		[]Option{},
		emu.OSC133Prompt,
		"first" + emu.OSC133CommandExec + "\n",
		"foo\n" + emu.OSC133CommandDone,
		emu.OSC133Prompt,
		"second" + emu.OSC133CommandExec + "\n",
		"bar\n" + emu.OSC133CommandDone1,
		emu.OSC133Prompt,
	)
}

func TestNoOutput(t *testing.T) {
	detectTest(
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
					To:   geom.Vec2{R: 1, C: 0},
				},
				HasExitCode:   true,
				ExitCode:      0,
				promptedWrite: 2,
				Prompted:      2,
				Executed:      3,
				Completed:     3,
			},
		},
		[]Option{},
		emu.OSC133Prompt,
		"command" + emu.OSC133CommandExec + "\n" + emu.OSC133CommandDone,
		emu.OSC133Prompt,
	)
}

// TestEndSameLine tests when output doesn't end with a newline.
func TestEndSameLine(t *testing.T) {
	detectTest(
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
					To:   geom.Vec2{R: 3, C: 3},
				},
				HasExitCode:   true,
				ExitCode:      0,
				promptedWrite: 2,
				Prompted:      2,
				Executed:      3,
				Completed:     6,
			},
		},
		[]Option{},
		emu.OSC133Prompt,
		"command" + emu.OSC133CommandExec + "\n",
		"foo\n",
		"bar\n",
		"baz" + emu.OSC133CommandDone, emu.OSC133Prompt,
	)
}

func TestDirectory(t *testing.T) {
	detectTest(
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
					To:   geom.Vec2{R: 3, C: 3},
				},
				HasExitCode:   true,
				ExitCode:      0,
				promptedWrite: 3,
				Prompted:      3,
				Executed:      4,
				Completed:     7,
			},
		},
		[]Option{},
		// set the directory with OSC-7
		"\033]7;test\033\\",
		emu.OSC133Prompt,
		"command" + emu.OSC133CommandExec + "\n",
		"foo\n",
		"bar\n",
		"baz\n" + emu.OSC133CommandDone,
		emu.OSC133Prompt,
	)
}

func TestDirectoryChange(t *testing.T) {
	detectTest(
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
				HasExitCode:   true,
				ExitCode:      0,
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
				HasExitCode:   true,
				ExitCode:      0,
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
				HasExitCode:   true,
				ExitCode:      0,
				promptedWrite: 10,
				Prompted:      10,
				Executed:      11,
				Completed:     12,
			},
		},
		[]Option{},
		// set dir before first
		"\033]7;test\033\\",
		emu.OSC133Prompt,
		"command" + emu.OSC133CommandExec + "\n",
		"foo\n" + emu.OSC133CommandDone,
		emu.OSC133Prompt,
		"command" + emu.OSC133CommandExec + "\n",
		// set dir during the second
		"\033]7;test2\033\\",
		"foo\n" + emu.OSC133CommandDone,
		// occurred in test2
		emu.OSC133Prompt,
		"command" + emu.OSC133CommandExec + "\n",
		"foo\n" + emu.OSC133CommandDone,
		emu.OSC133Prompt,
	)
}

func TestIgnored(t *testing.T) {
	detectTest(
		t,
		[]Command{},
		[]Option{},
		emu.OSC133Prompt,
		"command^C" + emu.OSC133CommandExec + "\n" + emu.OSC133CommandDone,
		emu.OSC133Prompt,
	)
}

func TestDirectoryProvider(t *testing.T) {
	var options = []Option{
		WithDirectoryProvider(func() string {
			return "foobar"
		}),
	}

	detectTest(
		t,
		[]Command{
			{
				Directory: "foobar",
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
				HasExitCode:   true,
				ExitCode:      0,
				promptedWrite: 3,
				Prompted:      3,
				Executed:      4,
				Completed:     7,
			},
		},
		options,
		"",
		emu.OSC133Prompt,
		"command" + emu.OSC133CommandExec + "\n",
		"foo\n",
		"bar\n",
		"baz\n" + emu.OSC133CommandDone,
		emu.OSC133Prompt,
	)
}

func TestMulti(t *testing.T) {
	detectTest(
		t,
		[]Command{
			{
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
				HasExitCode:   true,
				ExitCode:      0,
				promptedWrite: 2,
				Prompted:      2,
				Executed:      9,
				Completed:     10,
			},
		},
		[]Option{},
		emu.OSC133Prompt, "command\n",
		"> ", "foo\n",
		"> ", "\n",
		"> ", "baz" + emu.OSC133CommandExec + "\n",
		"output\n" + emu.OSC133CommandDone,
		emu.OSC133Prompt,
	)
}

func TestPending(t *testing.T) {
	detectTest(
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
				Pending:       true,
				promptedWrite: 2,
				Prompted:      2,
				Executed:      3,
				Completed:     4,
			},
		},
		[]Option{},
		emu.OSC133Prompt,
		"command" + emu.OSC133CommandExec + "\n",
		"foo",
	)
}

func TestPendingMulti(t *testing.T) {
	detectTest(
		t,
		[]Command{
			{
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
		},
		[]Option{},
		emu.OSC133Prompt, "command\n",
		"> ", "foo\n",
		"> ", "\n",
		"> ", "baz" + emu.OSC133CommandExec + "\n",
		"output\n",
	)
}
