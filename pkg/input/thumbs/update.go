package thumbs

import (
	"github.com/cfoust/cy/pkg/taro"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
)

func (t *Thumbs) resolveMatch(match Match) string {
	var (
		initial = t.initial
		runes   = make([]rune, 0)
	)
	for _, cell := range match {
		runes = append(runes,
			initial[cell.R][cell.C].Char,
		)
	}

	return string(runes)
}

func (t *Thumbs) handleKey(msg taro.KeyMsg) (taro.Model, tea.Cmd) {
	teaMsg := msg.ToTea()
	var cmds []tea.Cmd
	var cmd tea.Cmd

	// Update text input
	t.textInput, cmd = t.textInput.Update(teaMsg)
	cmds = append(cmds, cmd)

	// Get current input value after update
	input := strings.TrimSpace(t.textInput.Value())

	// Check if this input matches any hint exactly
	for hint, match := range t.hints {
		if hint != input {
			continue
		}

		// Exact match found - select it
		if t.result != nil {
			t.result <- t.resolveMatch(match)
		}

		return t.quit()
	}

	// Check if this input is a prefix of any hint
	hasValidPrefix := false
	if input != "" {
		for hint := range t.hints {
			if strings.HasPrefix(hint, input) {
				hasValidPrefix = true
				break
			}
		}

		// If input is not a valid prefix, reset the text input
		if !hasValidPrefix {
			t.textInput.SetValue("")
		}
	}

	return t, tea.Batch(cmds...)
}

func (t *Thumbs) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case taro.ScreenUpdate:
		return t, msg.Wait()
	case taro.KeyMsg:
		switch msg.Type {
		case taro.KeyEsc, taro.KeyCtrlC:
			if t.result != nil {
				t.result <- nil
			}
			return t.quit()
		}

		return t.handleKey(msg)
	}

	return t, nil
}
