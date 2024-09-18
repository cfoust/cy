package loader

import (
	"context"
	"os"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/sessions"
	"github.com/cfoust/cy/pkg/stories"
)

const (
	STORY_BORG_FILE = "/tmp/replay-loader-story.borg"
)

func createBorg() error {
	sim := sessions.NewSimulator().
		Defaults()

	for i := 0; i < 100_000; i++ {
		sim.Add("Finally, code is a cultural resource, not trivial and only instrumental, but bound up in social change, aesthetic projects, and the relationship of people to computers. Instead of being dismissed as cryptic and irrelevant to human concerns such as art and user experience, code should be valued as text with machine and human meanings, something produced and operating within culture.\n")
	}

	return sim.WriteBorg(STORY_BORG_FILE)
}

var Load stories.InitFunc = func(ctx context.Context) (mux.Screen, error) {
	if _, err := os.Stat(STORY_BORG_FILE); os.IsNotExist(err) {
		if err := createBorg(); err != nil {
			return nil, err
		}
	}

	return New(
		ctx,
		params.New(),
		bind.NewBindScope(nil),
		bind.NewBindScope(nil),
		STORY_BORG_FILE,
	), nil
}

func init() {
	config := stories.Config{
		Size: geom.DEFAULT_SIZE,
	}
	stories.Register(
		"replay/loader",
		Load,
		config,
	)
}
