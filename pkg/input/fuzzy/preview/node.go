package preview

import (
	"context"

	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/style"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type NodeType struct {
	Id       tree.NodeID
	ColorMap *style.ColorMap
}

type Node struct {
	util.Lifetime
	NodeType
	render     *taro.Renderer
	tree       *tree.Tree
	client     *server.Client
	colorMap   *style.ColorMap
	isAttached bool
}

var _ taro.Model = (*Node)(nil)

type AttachEvent struct {
	pane *tree.Pane
}

type DetachEvent struct{}

func (n *Node) Init() taro.Cmd {
	if n.tree == nil || n.client == nil {
		return nil
	}

	return func() tea.Msg {
		pane, ok := n.tree.PaneById(n.Id)
		if !ok {
			return nil
		}

		n.client.Attach(n.Ctx(), pane.Screen())
		return AttachEvent{pane: pane}
	}
}

func (n *Node) Update(msg taro.Msg) (taro.Model, taro.Cmd) {
	switch msg := msg.(type) {
	case taro.ScreenUpdate:
		return n, msg.Wait()
	case AttachEvent:
		n.isAttached = true
		w := taro.NewWatcher(n.Ctx(), msg.pane.Screen())
		return n, tea.Batch(
			func() tea.Msg {
				select {
				case <-n.client.Attachment().Ctx().Done():
					return nil
				case <-msg.pane.Ctx().Done():
					return DetachEvent{}
				}
			},
			w.Wait(),
		)
	case DetachEvent:
		n.isAttached = false
		return n, nil
	}

	return n, nil
}

func (f *Node) View(out *tty.State) {
	if !f.isAttached {
		preview := image.New(geom.DEFAULT_SIZE)
		f.render.RenderAt(
			preview,
			0, 0,
			lipgloss.Place(
				geom.DEFAULT_SIZE.C,
				geom.DEFAULT_SIZE.R,
				lipgloss.Center, lipgloss.Center,
				"attaching to pane",
			),
		)
		out.Image = preview
		return
	}

	isFiltered := f.colorMap != nil

	var state *tty.State
	if isFiltered {
		state = f.client.UnfilteredState()
	} else {
		state = f.client.State()
	}

	preview := image.New(state.Image.Size())
	image.Copy(geom.Vec2{}, preview, state.Image)

	// draw a ghost cursor
	cursor := state.Cursor
	if state.CursorVisible {
		style.GhostCursor(preview, cursor.R, cursor.C)
	}
	out.Image = preview
	if !isFiltered {
		return
	}

	f.colorMap.Apply(out.Image)
	return
}

func NewNode(
	ctx context.Context,
	tree *tree.Tree,
	client *server.Client,
	args NodeType,
) mux.Screen {
	l := util.NewLifetime(ctx)
	return taro.New(l.Ctx(), &Node{
		Lifetime: l,
		render:   taro.NewRenderer(),
		NodeType: args,
		tree:     tree,
		client:   client,
		colorMap: args.ColorMap,
	})
}
