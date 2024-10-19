# Split

{{story static layout/split-half}}

A `:split` node divides its visual space in two and gives it to two other nodes, rendering a line down the middle.

```janet
{
    :type :split
    :vertical false # boolean, optional
    :cells nil # int or nil, optional
    :percent nil # int or nil, optional
    :border :rounded # border type, dynamic, optional
    :border-fg nil # color, dynamic, optional
    :border-bg nil # color, dynamic, optional
    :a {} # a node
    :b {} # a node
}
```

`:vertical`

If `true`, the nodes are rendered on top of one another. If `false`, they are rendered side by side.

`:cells` and `:percent`

At most one of these can be defined. Both determine the amount of space given to the node described by `:a`. `:cells` is the number of cells along the split axis `:a` will be given; `:percent` is a percentage (`[0, 100]`) of the total size of the node along the split axis that will be allocated to `:a`.

`:border`

The [border style](#border-styles) to use for the dividing line.

`:border-fg` and `:border-bg`

The foreground and background [color](/api.md#color) of the border.

`:a` and `:b`

Both must be valid layout nodes.

### Dynamic

All dynamic properties are invoked with the same arguments:

1. The current value of `:a`.
1. The current value of `:b`.
