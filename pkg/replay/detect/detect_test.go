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

func promptSingle(
	t *testing.T,
	command Command,
	setup ...interface{},
) {
	promptTest(t, []Command{command}, []Option{}, setup...)
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
		[]Option{},
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
		[]Option{},
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

func TestIgnored(t *testing.T) {
	promptTest(
		t,
		[]Command{},
		[]Option{},
		TEST_PROMPT, "command^C\n",
		TEST_PROMPT,
	)
}

func TestDirectoryProvider(t *testing.T) {
	var options = []Option{
		WithDirectoryProvider(func() string {
			return "foobar"
		}),
	}

	promptTest(
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
				promptedWrite: 3,
				Prompted:      3,
				Executed:      4,
				Completed:     7,
			},
		},
		options,
		"",
		TEST_PROMPT, "command\n",
		"foo\n",
		"bar\n",
		"baz\n",
		TEST_PROMPT,
	)
}

// OSC 133 test prompt components
const (
	OSC133_PROMPT_START    = "\033]133;A\033\\"
	OSC133_COMMAND_START   = "\033]133;B\033\\"
	OSC133_COMMAND_EXEC    = "\033]133;C\033\\"
	OSC133_COMMAND_DONE    = "\033]133;D;0\033\\"
	OSC133_COMMAND_DONE_1  = "\033]133;D;1\033\\"
	OSC133_COMMAND_DONE_NO = "\033]133;D\033\\"
	// Realistic prompt: A marker + visible prompt + B marker
	OSC133_PROMPT = OSC133_PROMPT_START + "$ " + OSC133_COMMAND_START
)

// osc133Test is a test helper for OSC 133 detection tests
func osc133Test(
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
	// Note: no SetHooks for CY_HOOK - we're testing pure OSC 133
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
	require.True(t, d.useOSC133)
	require.Equal(t, commands, d.Commands(term, events))

	if d.commands != nil {
		require.Equal(t, d.commands, reported)
	}
}

// TestOSC133Simple tests basic command detection with OSC 133 sequences.
// The test bundles OSC 133 sequences with visible output to mimic real shell
// behavior where control sequences and visible text are sent together.
func TestOSC133Simple(t *testing.T) {
	osc133Test(
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
		OSC133_PROMPT,
		// User types command + C marker + newline (bundled)
		"command" + OSC133_COMMAND_EXEC + "\n",
		"foo\n",
		"bar\n",
		// Last output + D marker (bundled)
		"baz\n" + OSC133_COMMAND_DONE,
		// Next prompt
		OSC133_PROMPT,
	)
}

func TestOSC133ExitCodeNonZero(t *testing.T) {
	osc133Test(
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
		OSC133_PROMPT,
		"failing-cmd" + OSC133_COMMAND_EXEC + "\n",
		"error\n" + OSC133_COMMAND_DONE_1,
		OSC133_PROMPT,
	)
}

func TestOSC133NoExitCode(t *testing.T) {
	osc133Test(
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
		OSC133_PROMPT,
		"command" + OSC133_COMMAND_EXEC + "\n",
		"foo\n" + OSC133_COMMAND_DONE_NO,
		OSC133_PROMPT,
	)
}

func TestOSC133TwoCommands(t *testing.T) {
	osc133Test(
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
		OSC133_PROMPT,
		"first" + OSC133_COMMAND_EXEC + "\n",
		"foo\n" + OSC133_COMMAND_DONE,
		OSC133_PROMPT,
		"second" + OSC133_COMMAND_EXEC + "\n",
		"bar\n" + OSC133_COMMAND_DONE_1,
		OSC133_PROMPT,
	)
}

func TestOSC133NoOutput(t *testing.T) {
	osc133Test(
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
		OSC133_PROMPT,
		"command" + OSC133_COMMAND_EXEC + "\n" + OSC133_COMMAND_DONE,
		OSC133_PROMPT,
	)
}

func TestOSC133OverCYHOOK(t *testing.T) {
	// Test that OSC 133 takes precedence over CY_HOOK when both are present.
	// The CY_HOOK command is detected first, but then OSC 133 takes over.
	s := sessions.NewSimulator().
		Defaults()

	// First command using CY_HOOK
	s.AddTime(time.Second, TEST_PROMPT)
	s.AddTime(time.Second, "first\n")
	s.AddTime(time.Second, "output1\n")
	// OSC 133 prompt appears - this switches to OSC 133 mode
	s.AddTime(time.Second, OSC133_PROMPT)
	s.AddTime(time.Second, "second"+OSC133_COMMAND_EXEC+"\n")
	s.AddTime(time.Second, "output2\n"+OSC133_COMMAND_DONE)
	s.AddTime(time.Second, OSC133_PROMPT)

	events := s.Events()

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

	// Should have switched to OSC 133 mode
	require.True(t, d.useOSC133)

	commands := d.Commands(term, events)
	// Should have detected both commands
	require.Len(t, commands, 2)

	// First command detected via CY_HOOK (no exit code)
	require.Equal(t, "first", commands[0].Text)
	require.False(t, commands[0].HasExitCode)

	// Second command detected via OSC 133 (has exit code)
	require.Equal(t, "second", commands[1].Text)
	require.True(t, commands[1].HasExitCode)
	require.Equal(t, 0, commands[1].ExitCode)
}
