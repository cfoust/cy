package api

import (
	"fmt"
	"path/filepath"
	"strings"

	"github.com/cfoust/cy/pkg/janet"
	T "github.com/cfoust/cy/pkg/mux/screen/tree"
)

// validatePath ensures the provided path adheres to the API's path standard
// and splits it into parts.
func validatePath(path string) (parts []string, err error) {
	path = filepath.Clean(path)

	if path[0] != '/' {
		err = fmt.Errorf("path must start with /")
		return
	}

	parts = strings.Split(
		path,
		string('/'),
	)

	// Just the leading slash
	if len(parts) == 1 {
		err = fmt.Errorf("invalid path")
		return
	}

	return parts[1:], nil
}

// resolveNode resolves a Janet value to a node in `tree`. Allows use of either
// the :root keyword or integer NodeIDs.
func resolveNode(tree *T.Tree, target *janet.Value) (T.Node, error) {
	// first try keyword
	err := target.Unmarshal(&KEYWORD_ROOT)
	if err == nil {
		return tree.Root(), nil
	}

	// otherwise, node ID
	var id T.NodeID
	if err := target.Unmarshal(&id); err != nil {
		return nil, err
	}

	node, ok := tree.NodeById(id)
	if !ok {
		return nil, fmt.Errorf("node not found: %d", id)
	}

	return node, nil
}

func resolveGroup(tree *T.Tree, target *janet.Value) (*T.Group, error) {
	node, err := resolveNode(tree, target)
	if err != nil {
		return nil, err
	}

	group, ok := node.(*T.Group)
	if !ok {
		return nil, fmt.Errorf("node %d is not a group", node.Id())
	}

	return group, nil
}

func resolvePane(tree *T.Tree, target *janet.Value) (*T.Pane, error) {
	node, err := resolveNode(tree, target)
	if err != nil {
		return nil, err
	}

	pane, ok := node.(*T.Pane)
	if !ok {
		return nil, fmt.Errorf("node %d is not a pane", node.Id())
	}

	return pane, nil
}
