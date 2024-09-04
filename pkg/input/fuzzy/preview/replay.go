package preview

import (
	"context"

	"github.com/cfoust/cy/pkg/bind"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/replay/loader"
)

type ReplayType struct {
	Path string
}

func NewReplay(
	ctx context.Context,
	args ReplayType,
) mux.Screen {
	return loader.New(
		ctx,
		args.Path,
		bind.NewBindScope(nil),
		bind.NewBindScope(nil),
	)
}
