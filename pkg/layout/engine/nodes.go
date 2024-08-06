package engine

import (
	L "github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/layout/borders"
	"github.com/cfoust/cy/pkg/layout/margins"
	"github.com/cfoust/cy/pkg/layout/pane"
	"github.com/cfoust/cy/pkg/layout/split"
)

func (l *LayoutEngine) createPane(
	node *screenNode,
	config L.PaneType,
) error {
	pane := pane.New(
		node.Ctx(),
		l.tree,
		l.server,
		l.params,
	)

	pane.Apply(config)
	node.Screen = pane
	return nil
}

func (l *LayoutEngine) createMargins(
	node *screenNode,
	config L.MarginsType,
) error {
	marginsNode, err := l.createNode(
		node.Ctx(),
		config.Node,
	)
	if err != nil {
		return err
	}

	margins := margins.New(
		node.Ctx(),
		marginsNode.Screen,
	)

	margins.Apply(config)

	node.Screen = margins
	node.Children = []*screenNode{marginsNode}
	return nil
}

func (l *LayoutEngine) createSplit(
	node *screenNode,
	config L.SplitType,
) error {
	nodeA, err := l.createNode(node.Ctx(), config.A)
	if err != nil {
		return err
	}
	nodeB, err := l.createNode(node.Ctx(), config.B)
	if err != nil {
		return err
	}

	split := split.New(
		node.Ctx(),
		nodeA.Screen,
		nodeB.Screen,
		config.Vertical,
	)

	split.Apply(config)

	node.Screen = split
	node.Children = []*screenNode{nodeA, nodeB}
	return nil
}

func (l *LayoutEngine) createBorders(
	node *screenNode,
	config L.BorderType,
) error {
	innerNode, err := l.createNode(
		node.Ctx(),
		config.Node,
	)
	if err != nil {
		return err
	}

	borders := borders.New(
		node.Ctx(),
		innerNode.Screen,
	)

	borders.Apply(config)

	node.Screen = borders
	node.Children = []*screenNode{innerNode}
	return nil
}
