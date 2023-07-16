package tree

func findPath(current, needle Node) (result []Node) {
	if current == needle {
		result = append(result, current)
		return
	}

	switch node := current.(type) {
	case *Pane:
		return
	case *Group:
		for _, child := range node.Children() {
			path := findPath(child, needle)
			if len(path) == 0 {
				continue
			}

			result = append(
				[]Node{current},
				path...,
			)
			return
		}
	}

	return
}

func getLeaves(node Node) (result []Node) {
	switch node := node.(type) {
	case *Pane:
		result = append(result, node)
		return
	case *Group:
		for _, child := range node.Children() {
			result = append(
				result,
				getLeaves(child)...,
			)
			return
		}
	}

	return
}
