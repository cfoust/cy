package fuzzy

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

func (f *Fuzzy) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	var cmds []tea.Cmd
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case taro.ScreenUpdate:
		return f, msg.Wait()
	case matchResult:
		f.filtered = msg.Filtered
		return f, tea.Batch(
			f.setSelected(f.selected),
			f.emitOption(),
		)
	case tea.WindowSizeMsg:
		size := geom.Size{
			R: msg.Height,
			C: msg.Width,
		}
		if f.anim != nil {
			f.anim.Resize(size)
		}
		f.size = size
		f.location = geom.Vec2{
			R: geom.Clamp(f.location.R, 0, size.R-1),
			C: geom.Clamp(f.location.C, 0, size.C-1),
		}
	case taro.KeyMsg:
		switch msg.Type {
		case taro.KeyEsc, taro.KeyCtrlC:
			if f.result != nil {
				f.result <- nil
			}
			return f.quit()
		case taro.KeyHome, taro.KeyEnd:
			isTop := msg.Type == taro.KeyHome
			last := len(f.getOptions()) - 1

			var index int
			if (f.isUp && isTop) || (!f.isUp && !isTop) {
				index = last
			}

			return f, tea.Sequence(
				f.setSelected(index),
				f.emitOption(),
			)
		case taro.KeyPgUp, taro.KeyPgDown:
			delta := f.numRenderedOptions
			if delta == 0 {
				return f, nil
			}

			if msg.Type == taro.KeyPgUp {
				delta *= -1
			}

			if f.isUp {
				delta *= -1
			}

			return f, tea.Sequence(
				f.setSelected(f.selected+delta),
				f.emitOption(),
			)
		case taro.KeyDown, taro.KeyCtrlJ, taro.KeyUp, taro.KeyCtrlK:
			f.haveMoved = true
			upwards := false
			switch msg.Type {
			case taro.KeyUp, taro.KeyCtrlK:
				upwards = true
			}
			if f.isUp {
				upwards = !upwards
			}

			delta := -1
			if !upwards {
				delta = 1
			}

			return f, tea.Sequence(
				f.setSelected(f.selected+delta),
				f.emitOption(),
			)
		case taro.KeyEnter:
			if f.isSticky {
				return f, nil
			}

			if f.selected >= 0 && f.selected < len(f.getOptions()) {
				option := f.getOptions()[f.selected]
				f.result <- option.Result
			} else {
				f.result <- nil
			}
			return f.quit()
		}
	}

	inputMsg := msg
	// We need to translate taro.KeyMsg to tea.KeyMsg (for now)
	if key, ok := msg.(taro.KeyMsg); ok {
		inputMsg = key.ToTea()
	}
	f.textInput, cmd = f.textInput.Update(inputMsg)
	cmds = append(cmds, cmd)

	value := f.textInput.Value()
	if f.pattern != value {
		f.pattern = value
		cmds = append(cmds, queryOptions(
			f.options,
			value,
			f.caseSensitive,
		))
	}

	return f, tea.Batch(cmds...)
}
