package layout

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"

	"github.com/sasha-s/go-deadlock"
)

type Pane struct {
	deadlock.RWMutex
	*mux.UpdatePublisher

	tree   *tree.Tree
	server *server.Server

	size geom.Size
}

var _ mux.Screen = (*Pane)(nil)
var _ reusable = (*Pane)(nil)

func (p *Pane) Send(msg mux.Msg) {
}

func (p *Pane) Kill() {
}

func (p *Pane) State() *tty.State {
	state := tty.New(p.size)
	state.CursorVisible = false
	return state
}

func (p *Pane) Resize(size geom.Size) error {
	p.Lock()
	p.size = size
	p.Unlock()
	return nil
}

/*if node.ID == nil {*/
/*return frames.NewFramer(ctx, frames.RandomFrame()), nil*/
/*}*/

/*treeNode, ok := l.tree.NodeById(*node.ID)*/
/*if !ok {*/
/*return nil, fmt.Errorf(*/
/*"failed to find pane with ID %d",*/
/*node.ID,*/
/*)*/
/*}*/

/*pane, ok := treeNode.(*tree.Pane)*/
/*if !ok {*/
/*return nil, fmt.Errorf("node was not a pane")*/
/*}*/

/*client := l.server.AddClient(ctx, geom.DEFAULT_SIZE)*/
/*client.Attach(ctx, pane.Screen())*/
/*return client, nil*/

func (p *Pane) reuse(node NodeType) (bool, error) {
	_, ok := node.(PaneType)
	if !ok {
		return false, nil
	}

	return true, nil
}

func NewPane(
	ctx context.Context,
	tree *tree.Tree,
	server *server.Server,
) *Pane {
	p := &Pane{
		UpdatePublisher: mux.NewPublisher(),
		tree:            tree,
		server:          server,
	}

	return p
}
