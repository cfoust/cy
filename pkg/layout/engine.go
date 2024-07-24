package layout

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/frames"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/util"

	"github.com/sasha-s/go-deadlock"
)

type LayoutEngine struct {
	util.Lifetime
	deadlock.RWMutex
	*mux.UpdatePublisher

	tree   *tree.Tree
	server *server.Server

	size           geom.Size
	layout         Layout
	layoutLifetime *util.Lifetime
	screen         mux.Screen
}

var _ mux.Screen = (*LayoutEngine)(nil)

func (l *LayoutEngine) Kill() {
}

func (l *LayoutEngine) State() *tty.State {
	l.RLock()
	screen := l.screen
	l.RUnlock()

	if screen == nil {
		return tty.New(geom.DEFAULT_SIZE)
	}

	return screen.State()
}

func (l *LayoutEngine) Send(msg mux.Msg) {
}

func (l *LayoutEngine) Resize(size geom.Size) error {
	l.Lock()
	l.size = size
	screen := l.screen
	l.Unlock()

	if screen == nil {
		return nil
	}

	return screen.Resize(size)
}

func (l *LayoutEngine) createNode(
	ctx context.Context,
	type_ NodeType,
) (mux.Screen, error) {
	switch node := type_.(type) {
	case PaneType:
		if node.ID == nil {
			return frames.NewFramer(ctx, frames.RandomFrame()), nil
		}

		treeNode, ok := l.tree.NodeById(*node.ID)
		if !ok {
			return nil, fmt.Errorf(
				"failed to find pane with ID %d",
				node.ID,
			)
		}

		pane, ok := treeNode.(*tree.Pane)
		if !ok {
			return nil, fmt.Errorf("node was not a pane")
		}

		client := l.server.AddClient(ctx, geom.DEFAULT_SIZE)
		client.Attach(ctx, pane.Screen())
		return client, nil
	case MarginsType:
		screen, err := l.createNode(ctx, node.Node)
		if err != nil {
			return nil, err
		}
		return NewMargins(ctx, screen), nil
	case SplitType:
		screenA, err := l.createNode(ctx, node.A)
		if err != nil {
			return nil, err
		}
		screenB, err := l.createNode(ctx, node.B)
		if err != nil {
			return nil, err
		}

		split := NewSplit(
			ctx,
			screenA,
			screenB,
			node.Vertical,
		)
		split.SetAttached(
			isAttached(node.A),
			isAttached(node.B),
		)

		if node.Percent != nil {
			err := split.SetPercent(*node.Percent)
			if err != nil {
				return nil, err
			}
		}

		if node.Cells != nil {
			err := split.SetCells(*node.Cells)
			if err != nil {
				return nil, err
			}
		}

		return split, nil
	}

	return nil, fmt.Errorf("unimplemented screen")
}

func (l *LayoutEngine) Set(layout Layout) error {
	err := validateTree(layout.root)
	if err != nil {
		return err
	}

	l.Lock()
	defer l.Unlock()

	layoutLifetime := util.NewLifetime(l.Ctx())
	screen, err := l.createNode(layoutLifetime.Ctx(), layout.root)
	if err != nil {
		return err
	}

	screen.Resize(l.size)

	if l.layoutLifetime != nil {
		l.layoutLifetime.Cancel()
	}

	l.layoutLifetime = &layoutLifetime
	l.layout = layout
	l.screen = screen

	go func() {
		updates := screen.Subscribe(layoutLifetime.Ctx())
		for {
			select {
			case msg := <-updates.Recv():
				l.Publish(msg)
			case <-layoutLifetime.Ctx().Done():
				return
			}
		}
	}()
	return nil
}

func (l *LayoutEngine) Get() Layout {
	l.RLock()
	layout := l.layout
	l.RUnlock()
	return layout
}

func NewLayoutEngine(
	ctx context.Context,
	tree *tree.Tree,
	muxServer *server.Server,
) *LayoutEngine {
	lifetime := util.NewLifetime(ctx)
	engine := &LayoutEngine{
		Lifetime:        lifetime,
		UpdatePublisher: mux.NewPublisher(),
		tree:            tree,
		server:          muxServer,
	}

	return engine
}
