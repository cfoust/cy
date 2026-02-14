# doc: Group

(tree/group? node)

Return `true` if `node` is a group, `false` otherwise. `node` is a [NodeID](/api.md#nodeid).

# doc: Pane

(tree/pane? node)

Return `true` if `node` is a pane, `false` otherwise. `node` is a [NodeID](/api.md#nodeid).

# doc: SetName

(tree/set-name node name)

Set the name of `node` to `name`. `name` will be stripped of all whitespace and slashes. `node` is a [NodeID](/api.md#nodeid).

# doc: Name

(tree/name node)

Get the name of `node`, which is a [NodeID](/api.md#nodeid). This is the name of the node itself, _not_ its path.

# doc: Path

(tree/path node)

Get the path of `node`, which is a [NodeID](/api.md#nodeid).

# doc: Parent

(tree/parent node)

Get the [NodeID](/api.md#nodeid) for the parent of `node`. If `node` is `:root`, `(tree/parent)` returns `nil`.

# doc: Rm

(tree/rm node)

Remove the `node` and all of its child nodes. This will halt execution of any descendant panes. `node` is a [NodeID](/api.md#nodeid).

# doc: Id

(tree/id group path)

Get the [NodeID](/api.md#nodeid) of the node at the end of `path` from the perspective of `group`. Throws an error if any part of the path does not exist.

# doc: Root

Get the [NodeID](/api.md#nodeid) that corresponds to the root node.
