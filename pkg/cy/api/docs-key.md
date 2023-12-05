# doc: Bind

(key/bind target sequence callback)

Bind the key sequence `sequence` to `callback` in group `target`, which is a [NodeID](api.md#nodeid).

`sequence` is a [key sequence](./keybindings.md#key-sequences), which consists of a tuple with string elements that are either key literals (`"h"`), preset key specifiers (`"ctrl+a"`), or regex patterns (`[:re "^[a-z]$"]`).

Read more about binding keys in [the dedicated chapter](./keybindings.md).
