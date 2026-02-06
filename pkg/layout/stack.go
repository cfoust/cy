package layout

import (
	"context"
	"fmt"

	"github.com/cfoust/cy/pkg/emu"
	"github.com/cfoust/cy/pkg/geom"
	"github.com/cfoust/cy/pkg/geom/image"
	"github.com/cfoust/cy/pkg/geom/tty"
	"github.com/cfoust/cy/pkg/janet"
	"github.com/cfoust/cy/pkg/keys"
	"github.com/cfoust/cy/pkg/layout/prop"
	"github.com/cfoust/cy/pkg/mux"
	"github.com/cfoust/cy/pkg/mux/screen/server"
	"github.com/cfoust/cy/pkg/mux/screen/tree"
	"github.com/cfoust/cy/pkg/params"
	"github.com/cfoust/cy/pkg/style"
	"github.com/cfoust/cy/pkg/taro"

	"github.com/charmbracelet/lipgloss"
	"github.com/sasha-s/go-deadlock"
)

type Leaf struct {
	Active      bool
	Title       *prop.String
	TitleBottom *prop.String
	Border      *prop.Border
	BorderFg    *prop.Color
	BorderBg    *prop.Color
	Node        Node
}

type StackNode struct {
	Border   *prop.Border
	BorderFg *prop.Color
	BorderBg *prop.Color
	Leaves   []Leaf
}

var _ Node = (*StackNode)(nil)

func (n *StackNode) Type() NodeType {
	return NodeTypeStack
}

func (n *StackNode) IsAttached() bool {
	for _, leaf := range n.Leaves {
		if leaf.Node.IsAttached() {
			return true
		}
	}
	return false
}

func (n *StackNode) Children() (nodes []Node) {
	for _, leaf := range n.Leaves {
		nodes = append(nodes, leaf.Node)
	}
	return
}

func (n *StackNode) SetChild(index int, node Node) {
	if index < 0 || index >= len(n.Leaves) {
		return
	}
	n.Leaves[index].Node = node
}

func (n *StackNode) Clone() Node {
	cloned := *n
	leaves := make([]Leaf, 0, len(n.Leaves))
	for _, leaf := range n.Leaves {
		newLeaf := leaf
		newLeaf.Node = leaf.Node.Clone()
		leaves = append(leaves, newLeaf)
	}
	cloned.Leaves = leaves
	return &cloned
}

func (n *StackNode) Validate() error {
	if len(n.Leaves) == 0 {
		return fmt.Errorf(":stack must have at least one leaf")
	}

	activeCount := 0
	for _, leaf := range n.Leaves {
		if leaf.Active {
			activeCount++
		}
	}

	if activeCount == 0 {
		return fmt.Errorf(
			":stack must have exactly one active leaf",
		)
	}

	if activeCount > 1 {
		return fmt.Errorf(
			":stack must have exactly one active leaf, found %d",
			activeCount,
		)
	}

	for index, leaf := range n.Leaves {
		if leaf.Node == nil {
			return ErrChildNil
		}

		err := leaf.Node.Validate()
		if err == nil {
			continue
		}

		return fmt.Errorf(
			":stack index %d is invalid: %s",
			index,
			err,
		)
	}

	return nil
}

func (n *StackNode) MarshalJanet() interface{} {
	type leafArg struct {
		Active      bool
		Title       *prop.String
		TitleBottom *prop.String
		Border      *prop.Border
		BorderFg    *prop.Color
		BorderBg    *prop.Color
		Node        interface{}
	}
	type_ := struct {
		Type     janet.Keyword
		Border   *prop.Border
		BorderFg *prop.Color
		BorderBg *prop.Color
		Leaves   []leafArg
	}{
		Type:     NodeKeywordStack,
		Border:   n.Border,
		BorderFg: n.BorderFg,
		BorderBg: n.BorderBg,
	}

	for _, leaf := range n.Leaves {
		type_.Leaves = append(
			type_.Leaves,
			leafArg{
				Active:      leaf.Active,
				Title:       leaf.Title,
				TitleBottom: leaf.TitleBottom,
				Border:      leaf.Border,
				BorderFg:    leaf.BorderFg,
				BorderBg:    leaf.BorderBg,
				Node:        leaf.Node.MarshalJanet(),
			},
		)
	}

	return type_
}

func (n *StackNode) UnmarshalJanet(value *janet.Value) (Node, error) {
	type leafArg struct {
		Active      *bool
		Title       *prop.String
		TitleBottom *prop.String
		Border      *prop.Border
		BorderFg    *prop.Color
		BorderBg    *prop.Color
		Node        *janet.Value
	}
	type stackArgs struct {
		Border   *prop.Border
		BorderFg *prop.Color
		BorderBg *prop.Color
		Leaves   []leafArg
	}
	args := stackArgs{}
	err := value.Unmarshal(&args)
	if err != nil {
		return nil, err
	}

	type_ := StackNode{
		Border:   args.Border,
		BorderFg: args.BorderFg,
		BorderBg: args.BorderBg,
	}

	for i, leaf := range args.Leaves {
		newLeaf := Leaf{
			Title:       leaf.Title,
			TitleBottom: leaf.TitleBottom,
			Border:      leaf.Border,
			BorderFg:    leaf.BorderFg,
			BorderBg:    leaf.BorderBg,
		}

		if leaf.Active != nil {
			newLeaf.Active = *leaf.Active
		}

		if newLeaf.Border == nil {
			newLeaf.Border = defaultBorder
		}

		node, err := unmarshalNode(leaf.Node)
		if err != nil {
			return nil, fmt.Errorf(
				"stack leaf %d invalid: %s",
				i,
				err,
			)
		}

		newLeaf.Node = node
		type_.Leaves = append(type_.Leaves, newLeaf)
	}

	return &type_, nil
}

func (n *StackNode) VisibleChildren() (nodes []Node) {
	index := n.ActiveIndex()
	if index == -1 {
		return
	}
	return []Node{n.Leaves[index].Node}
}

func (n *StackNode) SetVisibleChild(index int, node Node) {
	if index != 0 {
		return
	}

	active := n.ActiveIndex()
	if active == -1 {
		return
	}

	n.Leaves[active].Node = node
}

func (n *StackNode) Screen(
	ctx context.Context,
	tree *tree.Tree,
	server *server.Server,
	params *params.Parameters,
	children []Reusable,
) Reusable {
	return NewStack(ctx, children[0])
}

func (n *StackNode) Active() (leaf Leaf) {
	for _, leaf := range n.Leaves {
		if leaf.Active {
			return leaf
		}
	}
	return
}

func (n *StackNode) ActiveIndex() int {
	for index, leaf := range n.Leaves {
		if leaf.Active {
			return index
		}
	}
	return -1
}

// Stack renders one active leaf with a full border box and collapsed
// leaves as single rows above/below it.
type Stack struct {
	*Computable
	deadlock.RWMutex
	*mux.UpdatePublisher
	render *taro.Renderer
	screen mux.Screen
	size   geom.Size
	inner  geom.Rect
	config *StackNode
}

var _ mux.Screen = (*Stack)(nil)
var _ Reusable = (*Stack)(nil)

func (s *Stack) Kill() {
	s.screen.Kill()
}

func (s *Stack) getLeafBorder(leaf Leaf) lipgloss.Border {
	if value, ok := leaf.Border.GetPreset(); ok {
		return value.Border
	}
	if value, ok := s.config.Border.GetPreset(); ok {
		return value.Border
	}
	return lipgloss.RoundedBorder()
}

func (s *Stack) getLeafBorderStyle(
	leaf Leaf,
) *style.Style {
	var borderFg, borderBg *style.Color

	if value, ok := leaf.BorderFg.GetPreset(); ok {
		borderFg = value
	} else if value, ok := s.config.BorderFg.GetPreset(); ok {
		borderFg = value
	}

	if value, ok := leaf.BorderBg.GetPreset(); ok {
		borderBg = value
	} else if value, ok := s.config.BorderBg.GetPreset(); ok {
		borderBg = value
	}

	return style.NewStyle(borderFg, borderBg)
}

func (s *Stack) renderCollapsedRow(
	state *tty.State,
	row int,
	leaf Leaf,
	isAbove bool,
) {
	size := state.Image.Size()
	border := s.getLeafBorder(leaf)
	colorStyle := s.getLeafBorderStyle(leaf)

	var leftChar, fillChar, rightChar rune
	if isAbove {
		leftChar = []rune(border.TopLeft)[0]
		fillChar = []rune(border.Top)[0]
		rightChar = []rune(border.TopRight)[0]
	} else {
		leftChar = []rune(border.BottomLeft)[0]
		fillChar = []rune(border.Bottom)[0]
		rightChar = []rune(border.BottomRight)[0]
	}

	if row < 0 || row >= size.R {
		return
	}

	// Fill the row with border chars
	for col := 0; col < size.C; col++ {
		state.Image[row][col].Char = fillChar
		colorStyle.Apply(&state.Image[row][col])
		state.Image[row][col].Write = emu.WriteID(0)
	}

	if size.C > 0 {
		state.Image[row][0].Char = leftChar
	}
	if size.C > 1 {
		state.Image[row][size.C-1].Char = rightChar
	}

	// Render title text on the row
	layout := New(leaf.Node)
	titleSize := geom.Vec2{
		R: 1,
		C: geom.Max(0, size.C-2),
	}
	if value, ok := leaf.Title.Get(
		s.Ctx(),
		s.Context.Context(),
		titleSize,
		&layout,
	); ok {
		s.render.RenderAt(
			state.Image,
			row, 1,
			s.render.NewStyle().
				MaxWidth(titleSize.C).
				Render(value),
		)
	}
}

func (s *Stack) State() *tty.State {
	s.Lock()
	defer s.Unlock()

	var (
		size   = s.size
		inner  = s.inner
		config = s.config
		screen = s.screen
	)

	state := tty.New(size)

	if config == nil || len(config.Leaves) == 0 {
		return state
	}

	activeIndex := config.ActiveIndex()
	if activeIndex == -1 {
		return state
	}

	activeLeaf := config.Leaves[activeIndex]

	// Render the active leaf's content into the inner rect
	innerState := screen.State()
	tty.Copy(inner.Position, state, innerState)

	// Render the active leaf's border box
	borderStyle := s.getLeafBorder(activeLeaf)

	boxStyle := s.render.NewStyle().
		Border(borderStyle).
		BorderForeground(lipgloss.Color("7")).
		BorderTop(true).
		BorderLeft(true).
		BorderRight(true).
		BorderBottom(true).
		Width(inner.Size.C).
		Height(inner.Size.R)

	if value, ok := activeLeaf.BorderFg.GetPreset(); ok {
		boxStyle = boxStyle.BorderForeground(value.Color)
	} else if value, ok := config.BorderFg.GetPreset(); ok {
		boxStyle = boxStyle.BorderForeground(value.Color)
	}
	if value, ok := activeLeaf.BorderBg.GetPreset(); ok {
		boxStyle = boxStyle.BorderBackground(value.Color)
	} else if value, ok := config.BorderBg.GetPreset(); ok {
		boxStyle = boxStyle.BorderBackground(value.Color)
	}

	// The border box starts at the row just above the inner content
	borderRow := inner.Position.R - 1
	s.render.RenderAt(
		state.Image,
		borderRow, 0,
		boxStyle.Render(""),
	)

	// Re-render inner content on top of the border box
	image.CopyRaw(inner.Position, state.Image, innerState.Image)

	// Render title on top border
	titleSize := geom.Vec2{
		R: 1,
		C: inner.Size.C,
	}
	layout := New(activeLeaf.Node)
	if value, ok := activeLeaf.Title.Get(
		s.Ctx(),
		s.Context.Context(),
		titleSize,
		&layout,
	); ok {
		s.render.RenderAt(
			state.Image,
			borderRow, 1,
			s.render.NewStyle().
				MaxWidth(inner.Size.C).
				Render(value),
		)
	}

	// Render title-bottom on bottom border
	bottomBorderRow := inner.Position.R + inner.Size.R
	if value, ok := activeLeaf.TitleBottom.Get(
		s.Ctx(),
		s.Context.Context(),
		titleSize,
		&layout,
	); ok {
		s.render.RenderAt(
			state.Image,
			bottomBorderRow, 1,
			s.render.NewStyle().
				MaxWidth(inner.Size.C).
				Render(value),
		)
	}

	// Render cursor from inner content
	state.CursorVisible = innerState.CursorVisible
	if innerState.CursorVisible {
		cursor := innerState.Cursor
		cursor.C += inner.Position.C
		cursor.R += inner.Position.R
		state.Cursor = cursor
	}

	// Render collapsed leaves above
	for i := 0; i < activeIndex; i++ {
		s.renderCollapsedRow(state, i, config.Leaves[i], true)
		// Write leaf index for click detection
		for col := 0; col < size.C; col++ {
			state.Image[i][col].Write = emu.WriteID(i)
		}
	}

	// Render collapsed leaves below
	belowStart := bottomBorderRow + 1
	for i := activeIndex + 1; i < len(config.Leaves); i++ {
		row := belowStart + (i - activeIndex - 1)
		s.renderCollapsedRow(
			state,
			row,
			config.Leaves[i],
			false,
		)
		for col := 0; col < size.C; col++ {
			if row < size.R {
				state.Image[row][col].Write = emu.WriteID(i)
			}
		}
	}

	return state
}

func (s *Stack) Apply(node Node) (bool, error) {
	config, ok := node.(*StackNode)
	if !ok {
		return false, nil
	}

	s.Lock()
	defer s.Unlock()

	s.config = config

	activeLeaf := config.Active()
	layout := New(activeLeaf.Node)

	for _, p := range []prop.Presettable{
		config.Border,
		config.BorderFg,
		config.BorderBg,
	} {
		p.Preset(
			s.Ctx(),
			s.Context.Context(),
			&layout,
		)
		p.SetLogger(s.Logger)
	}

	for _, leaf := range config.Leaves {
		for _, p := range []prop.Presettable{
			leaf.Border,
			leaf.BorderFg,
			leaf.BorderBg,
		} {
			p.Preset(
				s.Ctx(),
				s.Context.Context(),
				&layout,
			)
			p.SetLogger(s.Logger)
		}

		for _, p := range []prop.Presettable{
			leaf.Title,
			leaf.TitleBottom,
		} {
			p.ClearCache()
			p.SetLogger(s.Logger)
		}
	}

	return true, nil
}

func (s *Stack) Send(msg mux.Msg) {
	s.RLock()
	var (
		inner  = s.inner
		size   = s.size
		config = s.config
	)
	s.RUnlock()

	mouseMsg, ok := msg.(taro.MouseMsg)
	if !ok {
		s.screen.Send(msg)
		return
	}

	if inner.Contains(mouseMsg.Vec2) {
		s.screen.Send(taro.TranslateMouseMessage(
			msg,
			-inner.Position.C,
			-inner.Position.R,
		))
		return
	}

	// Must be a click
	if mouseMsg.Type != keys.MousePress ||
		mouseMsg.Button != keys.MouseLeft ||
		mouseMsg.Down {
		return
	}

	// Check if click is within bounds
	if mouseMsg.R < 0 || mouseMsg.R >= size.R ||
		mouseMsg.C < 0 || mouseMsg.C >= size.C {
		return
	}

	activeIndex := config.ActiveIndex()
	if activeIndex == -1 {
		return
	}

	// Determine which collapsed row was clicked
	clickRow := mouseMsg.R
	numAbove := activeIndex
	numBelow := len(config.Leaves) - activeIndex - 1
	bottomBorderRow := numAbove + 1 + inner.Size.R
	belowStart := bottomBorderRow + 1

	var leafIndex int = -1
	if clickRow < numAbove {
		// Clicked on a collapsed leaf above
		leafIndex = clickRow
	} else if clickRow >= belowStart && clickRow < belowStart+numBelow {
		// Clicked on a collapsed leaf below
		leafIndex = activeIndex + 1 + (clickRow - belowStart)
	}

	if leafIndex < 0 || leafIndex >= len(config.Leaves) ||
		leafIndex == activeIndex {
		return
	}

	newConfig := config.Clone().(*StackNode)
	isAttached := newConfig.IsAttached()

	if isAttached {
		if detached, ok := Detach(newConfig).(*StackNode); ok {
			newConfig = detached
		}
	}

	var newLeaves []Leaf
	for index, leaf := range newConfig.Leaves {
		isActive := index == leafIndex
		node := leaf.Node

		if isActive && isAttached {
			node = AttachFirst(leaf.Node)
		}

		newLeaves = append(newLeaves, Leaf{
			Active:      isActive,
			Title:       leaf.Title,
			TitleBottom: leaf.TitleBottom,
			Border:      leaf.Border,
			BorderFg:    leaf.BorderFg,
			BorderBg:    leaf.BorderBg,
			Node:        node,
		})
	}

	newConfig.Leaves = newLeaves
	s.Publish(NodeChangeEvent{Config: newConfig})
}

func (s *Stack) Resize(size geom.Size) error {
	s.Lock()
	defer s.Unlock()

	s.size = size

	numLeaves := 0
	if s.config != nil {
		numLeaves = len(s.config.Leaves)
	}
	collapsedRows := geom.Max(0, numLeaves-1)

	// Active leaf gets: total rows - collapsed rows - 2 (top+bottom border)
	innerR := geom.Max(1, size.R-collapsedRows-2)
	innerC := geom.Max(1, size.C-2)

	activeIndex := 0
	if s.config != nil {
		activeIndex = s.config.ActiveIndex()
		if activeIndex == -1 {
			activeIndex = 0
		}
	}

	// Inner content starts after collapsed-above rows + top border
	s.inner = geom.Rect{
		Position: geom.Vec2{
			R: activeIndex + 1,
			C: 1,
		},
		Size: geom.Vec2{
			R: innerR,
			C: innerC,
		},
	}

	for _, p := range []prop.Presettable{
		s.config.Active().Title,
		s.config.Active().TitleBottom,
	} {
		p.ClearCache()
	}

	return s.screen.Resize(s.inner.Size)
}

func (s *Stack) poll(ctx context.Context) {
	updates := s.screen.Subscribe(ctx)

	for {
		select {
		case <-ctx.Done():
			return
		case event := <-updates.Recv():
			if _, ok := event.(NodeChangeEvent); ok {
				continue
			}
			s.Publish(event)
		}
	}
}

func NewStack(ctx context.Context, screen mux.Screen) *Stack {
	c := NewComputable(ctx)
	stack := &Stack{
		Computable:      c,
		UpdatePublisher: mux.NewPublisher(),
		screen:          screen,
		size:            geom.DEFAULT_SIZE,
		render:          taro.NewRenderer(),
		config:          &StackNode{},
	}

	go stack.poll(stack.Ctx())

	return stack
}
