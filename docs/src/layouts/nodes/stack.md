# Stack

{{story static layout/stack}}

A `:stack` node contains multiple leaves stacked vertically, only one of which is expanded at a time. The expanded leaf renders with a full border box (like a `:borders` node), while collapsed leaves just show their title.

```janet
{
    :type :stack
    :leaves @[] # list of leaves
}

# leaves look like this:
{
    :active false # boolean, optional
    :title nil # string or nil, dynamic, optional
    :title-bottom nil # string or nil, dynamic, optional
    :border :rounded # border type, dynamic, optional
    :border-fg nil # color, dynamic, optional
    :border-bg nil # color, dynamic, optional
    :node {} # a node
}
```

### The `:leaves` property

There are some important constraints on the `:leaves` property:

- You must provide at least one leaf.
- There must be exactly one leaf with `:active` set to `true`.

### Leaf properties

`:title` and `:title-bottom`

These strings will be rendered on the top and bottom borders of the active leaf, respectively. For collapsed leaves, `:title` is rendered on the collapsed row. `:title-bottom` is only visible when a leaf is active.

`:border`

The [border style](/layouts/appearance.md#borders) for this leaf.

`:border-fg` and `:border-bg`

The foreground and background [color](/api.md#color) of the border. Each leaf can have its own border style and colors.

`:node`

A valid layout node.

### Clicking

You can click on a collapsed leaf row to switch to that leaf.

### Actions

- {{api action/next-leaf}}
- {{api action/prev-leaf}}
- {{api action/new-stack-leaf}}
- {{api action/close-stack-leaf}}

### Dynamic

`:title` and `:title-bottom`

1. The dimensions of the space available to either property as a tuple, `[rows, cols]`. `rows` is always `1`, but this structure is preserved for consistency.
1. The current value of `:node` for the leaf.

All other properties:

1. The current value of `:node` for the active leaf.
