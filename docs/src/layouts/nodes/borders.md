# Borders

{{story static layout/border}}

A `:borders` node surrounds its child in borders and adds an optional title to the top or bottom.

```janet
{
    :type :borders
    :title nil # string or nil, dynamic, optional
    :title-bottom nil # string or nil, dynamic, optional
    :border :rounded # border type, dynamic, optional
    :border-fg nil # color, dynamic, optional
    :border-bg nil # color, dynamic, optional
    :node {} # a node
}
```

`:title` and `:title-bottom`

These strings will be rendered on the top and the bottom of the window, respectively.

`:border`

The [border style](#border-styles) for this node. `:none` is not supported.

`:border-fg` and `:border-bg`

The foreground and background [color](/api.md#color) of the border.

`:node`

A valid layout node.

### Actions

- {{api action/set-borders-title}}
- {{api action/set-borders-title-bottom}}

### Dynamic

`:title` and `:title-bottom`

1. The dimensions of the space available to either property as a tuple, `[rows, cols]`. `rows` is always `1`, but this structure is preserved for consistency.
1. The current value of `:node`.

All other properties:

1. The current value of `:node`.
