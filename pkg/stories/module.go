package stories

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/mux"
)

type Story func(context.Context) mux.Screen

var stories = make(map[string]Story)

func Register(name string, story Story) {
	stories[name] = story
}

func Initialize(ctx context.Context, name string) (mux.Screen, error) {
	start, ok := stories[name]
	if !ok {
		return nil, fmt.Errorf("missing story %s", name)
	}

	return start(ctx), nil
}
