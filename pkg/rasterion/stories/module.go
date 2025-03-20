package stories

import (
	"context"
	"time"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/anim/meta"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/mux"
	R "github.com/cfoust/cy/pkg/rasterion"
	"github.com/cfoust/cy/pkg/stories"
)

type Drawing func(c *R.Context, delta time.Duration)

type Drawer struct {
	drawing Drawing
	start   time.Time
	rCtx    *R.Context
}

var _ meta.Animation = (*Drawer)(nil)

func (d *Drawer) Init(start image.Image) {
	d.rCtx = R.New(start.Size())
	d.start = time.Now()
}

func (d *Drawer) Update(delta time.Duration) image.Image {
	r := d.rCtx
	r.Clear()
	d.drawing(r, time.Now().Sub(d.start))
	return r.Image()
}

func createStory(drawing Drawing) stories.InitFunc {
	return func(ctx context.Context) (mux.Screen, error) {
		animator := anim.NewAnimator(
			ctx,
			&Drawer{drawing: drawing},
			image.New(geom.DEFAULT_SIZE.Scalar(2)),
			anim.DEFAULT_FPS,
		)
		return animator, nil
	}
}

func registerStory(name string, drawing Drawing) {
	stories.Register(
		"rasterion/"+name,
		createStory(drawing),
		stories.Config{},
	)
}
