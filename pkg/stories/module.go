package stories

import (
	"context"
	"fmt"

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

type Story func(context.Context) mux.Screen

type _story struct {
	init   Story
	config Config
}

var stories = make(map[string]_story)

func Register(name string, story Story, config Config) {
	stories[name] = _story{
		init:   story,
		config: config,
	}
}

func Initialize(ctx context.Context, name string) (*taro.Program, error) {
	story, ok := stories[name]
	if !ok {
		return nil, fmt.Errorf("missing story %s", name)
	}

	screen := story.init(ctx)
	config := story.config
	if !config.Size.IsZero() {
		screen.Resize(config.Size)
	}

	return NewViewer(
		ctx,
		screen,
		story.config,
	), nil
}
