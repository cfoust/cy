package thumbs

import (
	"strings"

	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

func (t *Thumbs) Update(msg tea.Msg) (taro.Model, tea.Cmd) {
	var cmds []tea.Cmd
	var cmd tea.Cmd

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
		case taro.KeyEnter:
			// If we have a partial hint typed, try to match it
			input := strings.TrimSpace(t.textInput.Value())
			if input != "" {
				// Check if this partial input matches any hint exactly
				for _, match := range t.matches {
					if match.Hint == input {
						if t.result != nil {
							t.result <- match.Text
						}
						return t.quit()
					}
				}
			}
			return t, nil
		}
	}

	// Convert taro key to tea key for text input
	inputMsg := msg
	if key, ok := msg.(taro.KeyMsg); ok {
		inputMsg = key.ToTea()
	}

	// Update text input
	t.textInput, cmd = t.textInput.Update(inputMsg)
	cmds = append(cmds, cmd)

	// Get current input value after update
	input := strings.TrimSpace(t.textInput.Value())

	// Check if this input matches any hint exactly
	for _, match := range t.matches {
		if match.Hint == input {
			// Exact match found - select it
			if t.result != nil {
				t.result <- match.Text
			}
			return t.quit()
		}
	}

	// Check if this input is a prefix of any hint
	hasValidPrefix := false
	if input != "" {
		for _, match := range t.matches {
			if strings.HasPrefix(match.Hint, input) {
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
