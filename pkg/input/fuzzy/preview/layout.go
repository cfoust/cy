package preview

import (
	"context"

	"github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/layout/engine"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
)

type LayoutType struct {
	Layout *layout.Layout
}

func NewLayout(
	ctx context.Context,
	tree *tree.Tree,
	muxServer *server.Server,
	args LayoutType,
) mux.Screen {
	l := engine.New(
		ctx,
		tree,
		muxServer,
	)

	// TODO(cfoust): 08/02/24 no error handling on this?
	l.Set(*args.Layout)
	return l
}
