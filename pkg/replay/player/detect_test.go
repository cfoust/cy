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
	require.Equal(t, commands, d.commands)
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
			Input: []search.Selection{
				{
					From: geom.Vec2{
						R: 0,
						C: 2,
					},
					To: geom.Vec2{
						R: 0,
						C: 9,
					},
				},
			},
			Output: search.Selection{
				To: geom.Vec2{
					R: 3,
					C: 2,
				},
			},
		},
		PROMPT,
		"command\n",
		"foo\nbar\nbaz\n",
		PROMPT,
	)
}

// Sometimes the output does not have a final \n, meaning that the prompt is
// printed on the last line of the output.
func TestEndSameLine(t *testing.T) {
	promptSingle(
		t,
		Command{
			Input: []search.Selection{
				{
					From: geom.Vec2{
						R: 0,
						C: 2,
					},
					To: geom.Vec2{
						R: 0,
						C: 9,
					},
				},
			},
			Output: search.Selection{
				To: geom.Vec2{
					R: 3,
					C: 2,
				},
			},
		},
		PROMPT,
		"command\n",
		"foo\nbar\nbaz",
		PROMPT,
	)
}
