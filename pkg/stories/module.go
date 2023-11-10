package stories

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/taro"
)

type Story func(context.Context) mux.Screen

var stories = make(map[string]Story)

func Register(name string, story Story) {
	stories[name] = story
}

func Initialize(ctx context.Context, name string) (*taro.Program, error) {
	_, ok := stories[name]
	if !ok {
		return nil, fmt.Errorf("missing story %s", name)
	}

	return NewViewer(ctx), nil
}
