package preview

import (
	"context"

	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
)

func New(
	ctx context.Context,
	tree *tree.Tree,
	client *server.Client,
	args interface{},
) mux.Screen {
	switch args := args.(type) {
	case NodeType:
		return NewNode(ctx, tree, client, args)
	case ReplayType:
		return NewReplay(ctx, args)
	case TextType:
		return NewText(ctx, args)
	case ScrollbackType:
		return NewScrollback(ctx, tree, args)
	}

	return nil
}
