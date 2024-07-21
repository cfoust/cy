package preview

import (
	"context"

	"github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/mux"
)

type FrameType struct {
	Name string
}

func NewFrame(
	ctx context.Context,
	args FrameType,
) mux.Screen {
	frame, ok := frames.Frames[args.Name]
	if !ok {
		frame = frames.Frames["none"]
	}

	return frames.NewFramer(ctx, frame)
}
