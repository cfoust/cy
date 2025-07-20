package text

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

func (t *Text) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	var cmds []tea.Cmd
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case taro.ScreenUpdate:
		return t, msg.Wait()
	case tea.WindowSizeMsg:
		size := geom.Size{
			R: msg.Height,
			C: msg.Width,
		}
		if t.anim != nil {
			t.anim.Resize(size)
		}
		t.size = size
		t.location = geom.Vec2{
			R: geom.Clamp(t.location.R, 0, size.R-1),
			C: geom.Clamp(t.location.C, 0, size.C-1),
		}
	case taro.KeyMsg:
		switch msg.Type {
		case taro.KeyEsc, taro.KeyCtrlC:
			if t.result != nil {
				t.result <- nil
			}
			return t.quit()
		case taro.KeyEnter:
			value := t.textInput.Value()
			if len(value) > 0 {
				t.result <- value
			} else {
				t.result <- nil
			}
			return t.quit()
		default:
			// In single character mode, accept any printable character immediately
			if t.isSingle && msg.Type == taro.KeyRunes {
				char := string(msg.Runes)
				if len(char) == 1 {
					t.result <- char
					return t.quit()
				}
			}
		}
	}

	// Only update the text input if not in single character mode
	if !t.isSingle {
		inputMsg := msg
		// We need to translate taro.KeyMsg to tea.KeyMsg (for now)
		if key, ok := msg.(taro.KeyMsg); ok {
			inputMsg = key.ToTea()
		}
		t.textInput, cmd = t.textInput.Update(inputMsg)
		cmds = append(cmds, cmd)
	}

	return t, tea.Batch(cmds...)
}
