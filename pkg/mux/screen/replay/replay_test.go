package replay

import (
	"testing"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/cfoust/cy/pkg/geom/tty"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/stretchr/testify/require"
	"github.com/xo/terminfo"
)

func createTestSession() []sessions.Event {
	s := sessions.NewSimulator()
	s.Add(
		"\033[20h", // CRLF -- why is this everywhere?
		geom.DEFAULT_SIZE,
		"test string please ignore",
	)
	s.Term(terminfo.ClearScreen)
	s.Add("take two")
	s.Term(terminfo.ClearScreen)
	s.Add("test")

	return s.Events()
}

func input(m taro.Model, msgs ...interface{}) taro.Model {
	var cmd tea.Cmd
	var realMsg tea.Msg
	for _, msg := range msgs {
		realMsg = msg
		switch msg := msg.(type) {
		case ActionType:
			realMsg = Action{Type: msg}
		case geom.Size:
			realMsg = tea.WindowSizeMsg{
				Width:  msg.C,
				Height: msg.R,
			}
		case string:
			keyMsgs := taro.KeysToMsg(msg)
			if len(keyMsgs) == 1 {
				realMsg = keyMsgs[0]
			}
		}

		m, cmd = m.Update(realMsg)
		for cmd != nil {
			m, cmd = m.Update(cmd())
		}
	}

	return m
}

func TestSearch(t *testing.T) {
	var r = newReplay(createTestSession(), bind.NewEngine[bind.Action]())
	input(r, ActionBeginning, ActionTimeSearchForward, "test", "enter")
	require.Equal(t, 2, len(r.matches))
}

func TestViewport(t *testing.T) {
	s := sessions.NewSimulator()
	s.Add(geom.Size{R: 20, C: 20})
	s.Term(terminfo.ClearScreen)
	s.Term(terminfo.CursorAddress, 19, 19)

	var r = newReplay(s.Events(), bind.NewEngine[bind.Action]())
	input(r, geom.Size{R: 10, C: 10})
	require.Equal(t, geom.Vec2{R: 0, C: 0}, r.minOffset)
	require.Equal(t, geom.Vec2{R: 10, C: 10}, r.maxOffset)
	require.Equal(t, geom.Vec2{R: 10, C: 10}, r.offset)
	r.View(tty.New(r.viewport))
}
