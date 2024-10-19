# Margins

{{story static layout/margins}}

A `:margins` node puts transparent margins around its child allowing the current frame to show through.

```janet
{
    :type :margins
    :cols 0 # number, optional
    :rows 0 # number, optional
    :border :rounded # border type, dynamic, optional
    :border-fg nil # color, dynamic, optional
    :border-bg nil # color, dynamic, optional
    :node {} # a node
}
```

`:cols` and `:rows`

These properties set the size of the node inside of the `:margins` node; they do not refer to the width of the margins. A value of 0 for either property means that the node will use the full space available to it in that dimension.

`:border`

The [border style](#border-styles) for the borders around the node.

`:border-fg` and `:border-bg`

The foreground and background [color](/api.md#color) of the border.

`:node`

A valid layout node.

### Dynamic

All dynamic properties are invoked with the same arguments:

1. The current value of `:node`.
