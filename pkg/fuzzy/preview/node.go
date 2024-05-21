package preview

import (
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/taro"
	"github.com/cfoust/cy/pkg/util"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type NodeType struct {
	Id tree.NodeID
}

type Node struct {
	util.Lifetime
	NodeType
	render     *taro.Renderer
	tree       *tree.Tree
	client     *server.Client
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
	case AttachEvent:
		n.isAttached = true
		return n, func() tea.Msg {
			select {
			case <-n.client.Attachment().Ctx().Done():
				return nil
			case <-msg.pane.Ctx().Done():
				return DetachEvent{}
			}
		}
	case DetachEvent:
		n.isAttached = false
		return n, nil
	}

	return n, nil
}

func (f *Node) View(out *tty.State) {
	if f.isAttached {
		state := f.client.State()
		preview := image.New(state.Image.Size())
		image.Copy(geom.Vec2{}, preview, state.Image)

		// draw a ghost cursor
		cursor := state.Cursor
		if state.CursorVisible {
			preview[cursor.R][cursor.C].BG = 8
		}
		out.Image = preview
		return
	}

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
