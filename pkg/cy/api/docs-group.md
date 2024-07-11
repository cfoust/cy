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

# doc: Mkdir

(group/mkdir group path)

Get the group at the end of `path` from the perspective of `group`, creating any groups that do not exist. Returns the [NodeID](api.md#nodeid) of the final group.

For example:

```janet
(group/mkdir :root "/foo/bar/baz")
# Returns the NodeID of "baz"
```

This function throws an error if any node in the path already exists and is not a group.
