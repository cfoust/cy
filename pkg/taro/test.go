package taro

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"

	"github.com/charmbracelet/bubbles/cursor"
	tea "github.com/charmbracelet/bubbletea"
)

func testExecuteCommands(
	ctx context.Context,
	m Model,
	size geom.Size,
	cmds []tea.Cmd,
	options ...Option,
) geom.Size {
	p := newProgram(ctx, m)

	for _, option := range options {
		option(p)
	}

	p.isTest = true

	p.resize(size)
	clearc := make(chan struct{})
	p.clear = clearc

	// Skip Blink messages, since they repeat forever
	p.filter = func(msg tea.Msg) tea.Msg {
		if _, ok := msg.(cursor.BlinkMsg); ok {
			return nil
		}
		if _, ok := msg.(ScreenUpdate); ok {
			return nil
		}
		return msg
	}

	done := make(chan struct{})
	go func() {
		_, _ = p.Run()
		done <- struct{}{}
	}()

	p.Send(sequenceMsg(cmds))

	<-p.clear
	p.clear = nil
	p.Send(tea.QuitMsg{})
	<-done
	return p.state.Image.Size()
}

// Test returns a function that passes the given messages to the taro Model `m`
// one at a time. All `Cmd`s that would normally be executed in separate
// goroutines are instead executed in the calling goroutine. This is useful
// exclusively in testing.
func Test(m Model, options ...Option) func(msgs ...interface{}) {
	size := geom.DEFAULT_SIZE

	ctx, cancel := context.WithCancel(context.Background())
	size = testExecuteCommands(ctx, m, size, []tea.Cmd{m.Init()}, options...)
	cancel()

	return func(msgs ...interface{}) {
		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()

		cmds := []tea.Cmd{}

		var realMsg Msg
		for _, msg := range msgs {
			realMsg = msg
			switch msg := msg.(type) {
			case geom.Size:
				newSize := msg
				cmds = append(cmds, func() tea.Msg {
					return tea.WindowSizeMsg{
						Width:  newSize.C,
						Height: newSize.R,
					}
				})
				continue
			case string:
				keyMsgs := KeysToMsg(msg)
				if len(keyMsgs) == 1 {
					realMsg = keyMsgs[0]
				}
			}

			cmds = append(cmds, func(msg tea.Msg) tea.Cmd {
				return func() tea.Msg {
					return msg
				}
			}(realMsg))
		}

		size = testExecuteCommands(ctx, m, size, cmds, options...)
	}
}
