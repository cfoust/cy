package preview

import (
	"context"

	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"
)

func New(
	ctx context.Context,
	tree *tree.Tree,
	client *server.Client,
	args interface{},
) mux.Screen {
	l := util.NewLifetime(ctx)
	switch args := args.(type) {
	case NodeType:
		return taro.New(l.Ctx(), &Node{
			Lifetime: l,
			render:   taro.NewRenderer(),
			NodeType: args,
			tree:     tree,
			client:   client,
		})
	case ReplayType:
		return taro.New(l.Ctx(), &Replay{
			Lifetime:   l,
			render:     taro.NewRenderer(),
			ReplayType: args,
		})
	}

	return nil
}
