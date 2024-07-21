package preview

import (
	"context"

	"github.com/cfoust/cy/pkg/anim"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/mux"
)

type AnimationType struct {
	Name string
}

func NewAnimation(
	ctx context.Context,
	initial image.Image,
	args AnimationType,
) mux.Screen {
	animation, ok := anim.Animations[args.Name]
	if !ok {
		animation = anim.Animations["midjo"]
	}

	return anim.NewAnimator(
		ctx,
		animation(),
		initial,
		anim.DEFAULT_FPS,
	)
}
