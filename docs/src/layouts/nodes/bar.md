# Bar

{{story cast layout/dynamic/bar}}

A `:bar` node is a status bar shown above or below the node that it contains.

```janet
{
    :type :bar
    :bottom false # boolean, optional
    :text nil # string, dynamic
    :node {} # a node
}
```

`:bottom`

If `true`, the bar will be on the bottom of the node instead of the top.

`:text`

The contents of the bar.

`:node`

A valid layout node.

### Dynamic

1. The dimensions of the space available to either property as a tuple, `[rows cols]`. `rows` is always `1`, but this structure is preserved for consistency.
1. The current value of `:node`.
