# doc: Get

(param/get key &named target)

Get the value of the [parameter](./parameters.md) with `key`, which must be a keyword.

Parameter values are retrieved in the following order:

1. The client's parameter table, which overrides all other parameters and can be set with `(param/set :client ...)`.
2. The parameter table for the node the client is attached to and every parent node's parameter table, recursively, until the root node is reached.
3. The default value for [default parameters](parameters.md#default-parameters).

If `target` is provided, it must be either `:client` or a [NodeID](api.md#nodeid). This can be used to get parameter values from the perspective of another node in the tree, since it does the same cascade that `(param/get)` does normally.

# doc: Set

(param/set target key value)

Set the value of the [parameter](./parameters.md) at `target` for `key` to `value`. `target` must be either `:client` or a [NodeID](api.md#nodeid) (such as `:root`.) `value` can be any Janet value, though `(param/set)` does enforce the type of [default parameters](parameters.md#default-parameters).
