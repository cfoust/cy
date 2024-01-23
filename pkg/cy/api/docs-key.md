# doc: Bind

(key/bind target sequence callback)

Bind the key sequence `sequence` to `callback` for node `target`, which is a [NodeID](api.md#nodeid) or `:replay`. `target` can refer to any group or pane.

`sequence` is a [key sequence](./keybindings.md#key-sequences), which consists of a tuple with string elements that are either key literals (`"h"`), preset key specifiers (`"ctrl+a"`), or regex patterns (`[:re "^[a-z]$"]`).

Read more about binding keys in [the dedicated chapter](./keybindings.md).

# doc: Unbind

(key/unbind target sequence)

Clear all bindings that begin with `sequence` for node `target`, which is a [NodeID](api.md#nodeid) or `:replay`. Note that the empty sequence `[]` will unbind all keys in the scope.

`sequence` is a [key sequence](./keybindings.md#key-sequences), which consists of a tuple with string elements that are either key literals (`"h"`), preset key specifiers (`"ctrl+a"`), or regex patterns (`[:re "^[a-z]$"]`).

# doc: Remap

(key/remap target from to)

Remap all bindings that begin with sequence `from` to sequence `to` for node `target`, which is a [NodeID](api.md#nodeid) or `:replay`. Empty sequences (`[]`) are not currently supported for `from` and `to`.

For example, to remap all of the default bindings that begin with `ctrl+a` to `ctrl+v`:

```janet
(key/remap :root ["ctrl+a"] ["ctrl+v"])
```
