package stories

import (
	"context"
	"sort"
	"strings"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"

	tea "github.com/charmbracelet/bubbletea"
)

type Config struct {
	// If zero, the story will resize
	Size geom.Size
	// If true, the viewer captures the screen immediately and uses that
	// instead of a live view.
	IsSnapshot bool
}

type InitFunc func(context.Context) mux.Screen

type Story struct {
	Name   string
	Init   InitFunc
	config Config
}

var Stories = make(map[string]Story)

func Register(name string, init InitFunc, config Config) {
	Stories[name] = Story{
		Name:   name,
		Init:   init,
		config: config,
	}
}

func Initialize(ctx context.Context, filter string) (*taro.Program, error) {
	filteredStories := make([]Story, 0)
	for _, story := range Stories {
		if !strings.HasPrefix(story.Name, filter) {
			continue
		}
		filteredStories = append(filteredStories, story)
	}

	sort.SliceStable(filteredStories, func(i, j int) bool {
		return filteredStories[i].Name < filteredStories[j].Name
	})

	return NewBrowser(
		ctx,
		filteredStories,
	), nil
}

func Send(s mux.Screen, msgs ...interface{}) {
	var realMsg tea.Msg
	for _, msg := range msgs {
		realMsg = msg
		switch msg := msg.(type) {
		case string:
			keyMsgs := taro.KeysToMsg(msg)
			if len(keyMsgs) == 1 {
				realMsg = keyMsgs[0]
			}
		}
		s.Send(realMsg)
	}
}
