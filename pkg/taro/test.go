package taro

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"

	"github.com/charmbracelet/bubbles/cursor"
	tea "github.com/charmbracelet/bubbletea"
)

// Test returns a function that passes the given messages to the taro Model `m`
// one at a time. All `Cmd`s that would normally be executed in separate
// goroutines are instead executed in the calling goroutine. This is useful
// exclusively in testing.
func Test(m Model) func(msgs ...interface{}) {
	size := geom.DEFAULT_SIZE

	return func(msgs ...interface{}) {
		var cmd, subCmd Cmd
		var realMsg Msg

		var handleCmd func(Cmd)
		handleCmd = func(cmd Cmd) {
			for cmd != nil {
				msg := cmd()

				switch msg := msg.(type) {
				case cursor.BlinkMsg:
				case tea.BatchMsg:
					for _, cmd := range msg {
						m, subCmd = m.Update(cmd())
						handleCmd(subCmd)
					}
					cmd = nil
				default:
					m, cmd = m.Update(msg)
				}
				m.View(tty.New(size))
			}
		}

		for _, msg := range msgs {
			realMsg = msg
			switch msg := msg.(type) {
			case geom.Size:
				size = msg
				realMsg = tea.WindowSizeMsg{
					Width:  msg.C,
					Height: msg.R,
				}
			case string:
				keyMsgs := KeysToMsg(msg)
				if len(keyMsgs) == 1 {
					realMsg = keyMsgs[0]
				}
			}

			m, cmd = m.Update(realMsg)
			handleCmd(cmd)
			m.View(tty.New(size))
		}
	}
}
