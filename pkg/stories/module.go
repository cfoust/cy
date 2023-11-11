package stories

import (
	"context"
	"strings"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"
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
	init   InitFunc
	config Config
}

var stories = make(map[string]Story)

func Register(name string, init InitFunc, config Config) {
	stories[name] = Story{
		Name:   name,
		init:   init,
		config: config,
	}
}

func Initialize(ctx context.Context, filter string) (*taro.Program, error) {
	filteredStories := make([]Story, 0)
	for _, story := range stories {
		if !strings.HasPrefix(story.Name, filter) {
			continue
		}
		filteredStories = append(filteredStories, story)
	}

	return NewBrowser(
		ctx,
		filteredStories,
	), nil
}
