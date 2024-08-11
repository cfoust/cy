package engine

import (
	"context"
	"fmt"

	L "github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/layout/bar"
	"github.com/cfoust/cy/pkg/layout/borders"
	"github.com/cfoust/cy/pkg/layout/margins"
	"github.com/cfoust/cy/pkg/layout/pane"
	"github.com/cfoust/cy/pkg/layout/split"
	"github.com/cfoust/cy/pkg/layout/tabs"
	"github.com/cfoust/cy/pkg/util"

	"github.com/rs/zerolog/log"
)

// createNode takes a layout node and (recursively) creates a new screenNode
// that corresponds to that layout node's configuration.
func (l *LayoutEngine) createNode(
	ctx context.Context,
	config L.NodeType,
) (*screenNode, error) {
	nodeLifetime := util.NewLifetime(ctx)
	node := &screenNode{
		Lifetime: nodeLifetime,
		Config:   config,
	}

	var err error
	switch config := config.(type) {
	case L.PaneType:
		err = l.createPane(node, config)
	case L.MarginsType:
		err = l.createMargins(node, config)
	case L.SplitType:
		err = l.createSplit(node, config)
	case L.BorderType:
		err = l.createBorders(node, config)
	case L.TabsType:
		err = l.createTabs(node, config)
	case L.BarType:
		err = l.createBar(node, config)
	default:
		err = fmt.Errorf("unimplemented screen")
	}

	if err != nil {
		return nil, err
	}

	if log, ok := node.Screen.(L.Loggable); ok {
		log.SetLogger(l.log)
	}

	if c, ok := node.Screen.(L.Contextable); ok {
		c.SetContext(l.context)
	}

	if r, ok := node.Screen.(L.Reusable); ok {
		r.Apply(config)
	}

	go func() {
		updates := node.Screen.Subscribe(node.Ctx())
		for {
			select {
			case msg := <-updates.Recv():
				switch msg := msg.(type) {
				case L.NodeChangeEvent:
					err := l.handleChange(node, msg.Config)
					if err == nil {
						continue
					}

					log.Debug().Msgf("node %+v had invalid NodeChangeEvent: %s", node, err)
				case L.NodeRemoveEvent:
					l.removeAttached()
				}
			case <-node.Ctx().Done():
				return
			}
		}
	}()

	return node, err
}

type updateNode struct {
	Config L.NodeType
	Node   *screenNode
}

// updateNode attempts to reuse the given screenNode to match the provided
// layout node's configuration if it is possible, or creates a new screenNode
// if it is not.
func (l *LayoutEngine) updateNode(
	ctx context.Context,
	config L.NodeType,
	current *screenNode,
) (*screenNode, error) {
	if current == nil {
		return l.createNode(ctx, config)
	}

	canReuse, err := current.Screen.Apply(config)
	if err != nil {
		return nil, err
	}

	if !canReuse {
		current.Cancel()
		return l.createNode(ctx, config)
	}

	var updates []updateNode
	switch node := config.(type) {
	case L.SplitType:
		updates = append(updates,
			updateNode{
				Config: node.A,
				Node:   current.Children[0],
			},
			updateNode{
				Config: node.B,
				Node:   current.Children[1],
			},
		)
	case L.MarginsType:
		updates = append(updates,
			updateNode{
				Config: node.Node,
				Node:   current.Children[0],
			},
		)
	case L.BorderType:
		updates = append(updates,
			updateNode{
				Config: node.Node,
				Node:   current.Children[0],
			},
		)
	case L.TabsType:
		updates = append(updates,
			updateNode{
				Config: node.Active().Node,
				Node:   current.Children[0],
			},
		)
	case L.BarType:
		updates = append(updates,
			updateNode{
				Config: node.Node,
				Node:   current.Children[0],
			},
		)
	}

	// If any of the node's children cannot be reused, we need to remake
	// the whole node. Theoretically, you could (at the cost of a lot of
	// complexity) remake only the nodes that changed, but this would
	// require careful retooling of all of the node screens to handle
	// partially changing the screens they subscribe to.
	var children []*screenNode
	for _, update := range updates {
		child, err := l.updateNode(
			current.Ctx(),
			update.Config,
			update.Node,
		)
		if err != nil {
			return nil, err
		}

		if update.Node.Screen != child.Screen {
			current.Cancel()
			return l.createNode(ctx, config)
		}

		children = append(children, child)
	}

	current.Config = config
	current.Children = children
	return current, nil
}

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

	node.Screen = borders
	node.Children = []*screenNode{innerNode}
	return nil
}

func (l *LayoutEngine) createTabs(
	node *screenNode,
	config L.TabsType,
) error {
	innerNode, err := l.createNode(
		node.Ctx(),
		config.Active().Node,
	)
	if err != nil {
		return err
	}

	tabs := tabs.New(
		node.Ctx(),
		innerNode.Screen,
	)

	node.Screen = tabs
	node.Children = []*screenNode{innerNode}
	return nil
}

func (l *LayoutEngine) createBar(
	node *screenNode,
	config L.BarType,
) error {
	innerNode, err := l.createNode(
		node.Ctx(),
		config.Node,
	)
	if err != nil {
		return err
	}

	bar := bar.New(
		node.Ctx(),
		innerNode.Screen,
	)

	node.Screen = bar
	node.Children = []*screenNode{innerNode}
	return nil
}

// applyNodeChange replaces the configuration of the target node with
// newConfig. This is only used to allow nodes to change their own
// configurations in response to user input (for now, just mouse events.)
func applyNodeChange(
	current, target *screenNode,
	currentConfig, newConfig L.NodeType,
) L.NodeType {
	if current == target {
		return newConfig
	}

	switch currentConfig := currentConfig.(type) {
	case L.PaneType:
		return currentConfig
	case L.SplitType:
		currentConfig.A = applyNodeChange(
			current.Children[0],
			target,
			currentConfig.A,
			newConfig,
		)
		currentConfig.B = applyNodeChange(
			current.Children[1],
			target,
			currentConfig.B,
			newConfig,
		)
		return currentConfig
	case L.MarginsType:
		currentConfig.Node = applyNodeChange(
			current.Children[0],
			target,
			currentConfig.Node,
			newConfig,
		)
		return currentConfig
	case L.BorderType:
		currentConfig.Node = applyNodeChange(
			current.Children[0],
			target,
			currentConfig.Node,
			newConfig,
		)
		return currentConfig
	case L.TabsType:
		for i, tab := range currentConfig.Tabs {
			if !tab.Active {
				continue
			}

			currentConfig.Tabs[i].Node = applyNodeChange(
				current.Children[0],
				target,
				currentConfig.Tabs[i].Node,
				newConfig,
			)
			return currentConfig
		}
	case L.BarType:
		currentConfig.Node = applyNodeChange(
			current.Children[0],
			target,
			currentConfig.Node,
			newConfig,
		)
		return currentConfig
	}

	return currentConfig
}
