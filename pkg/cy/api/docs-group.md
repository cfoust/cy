# doc: New

(group/new parent &named name)

Create a new group with `parent` and (optionally) `name`.

`parent` is a [NodeID](api.md#nodeid).

# doc: Children

(group/children group)

Get the [NodeID](api.md#nodeid)s for all of `group`'s child nodes.

# doc: Leaves

(group/leaves group)

Get the [NodeID](api.md#nodeid)s for all of the leaf nodes reachable from `group`. In other words, this is a list of all of the [NodeID](api.md#nodeid)s for all panes that are descendants of `group`.
