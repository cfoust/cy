package engine

import (
	"context"
	"fmt"

	L "github.com/cfoust/cy/pkg/layout"
	"github.com/cfoust/cy/pkg/util"

	"github.com/rs/zerolog/log"
)

// createNode takes a layout node and (recursively) creates a new screenNode
// that corresponds to that layout node's configuration.
func (l *LayoutEngine) createNode(
	ctx context.Context,
	config L.Node,
) (*screenNode, error) {
	nodeLifetime := util.NewLifetime(ctx)
	node := &screenNode{
		Lifetime: nodeLifetime,
		Config:   config,
	}

	var (
		childNodes = config.VisibleChildren()
		screens    = []L.Reusable{}
		children   = []*screenNode{}
	)

	for i, child := range childNodes {
		childNode, err := l.createNode(ctx, child)
		if err != nil {
			return nil, fmt.Errorf(
				"error creating child %d: %w",
				i,
				err,
			)
		}

		children = append(children, childNode)
		screens = append(screens, childNode.Screen)
	}

	node.Children = children
	node.Screen = config.Screen(
		node.Ctx(),
		l.tree,
		l.server,
		l.params,
		screens,
	)

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

	return node, nil
}

type updateNode struct {
	Config L.Node
	Node   *screenNode
}

// updateNode attempts to reuse the given screenNode to match the provided
// layout node's configuration if it is possible, or creates a new screenNode
// if it is not.
func (l *LayoutEngine) updateNode(
	ctx context.Context,
	current *screenNode,
	node L.Node,
) (*screenNode, error) {
	if current == nil {
		return l.createNode(ctx, node)
	}

	canReuse, err := current.Screen.Apply(node)
	if err != nil {
		return nil, err
	}

	if !canReuse {
		current.Cancel()
		return l.createNode(ctx, node)
	}

	var (
		childNodes     = node.VisibleChildren()
		oldScreenNodes = current.Children
		newScreenNodes []*screenNode
	)

	if len(oldScreenNodes) != len(childNodes) {
		return nil, fmt.Errorf(
			"invalid number of screenNodes: %d vs %d",
			len(oldScreenNodes),
			len(childNodes),
		)
	}

	// If any of the node's screenNodes cannot be reused, we need to remake
	// the whole node. Theoretically, you could (at the cost of a lot of
	// complexity) remake only the nodes that changed, but this would
	// require careful retooling of all of the node screens to handle
	// partially changing the screens they subscribe to.
	for i, oldNode := range oldScreenNodes {
		newNode, err := l.updateNode(
			current.Ctx(),
			oldNode,
			childNodes[i],
		)
		if err != nil {
			return nil, err
		}

		if oldNode.Screen != newNode.Screen {
			current.Cancel()
			return l.createNode(ctx, node)
		}

		newScreenNodes = append(newScreenNodes, newNode)
	}

	current.Config = node
	current.Children = newScreenNodes
	return current, nil
}

// applyNodeChange replaces the configuration of the target node with
// newConfig. This is only used to allow nodes to change their own
// configurations in response to user input (for now, just mouse events.)
func applyNodeChange(
	current, target *screenNode,
	currentConfig, newConfig L.Node,
) L.Node {
	if current == target {
		return newConfig
	}

	var (
		childNodes  = currentConfig.VisibleChildren()
		screenNodes = current.Children
	)
	for i, child := range screenNodes {
		currentConfig.SetVisibleChild(
			i,
			applyNodeChange(
				child,
				target,
				childNodes[i],
				newConfig,
			),
		)
	}

	return currentConfig
}
