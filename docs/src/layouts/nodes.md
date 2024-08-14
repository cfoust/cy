# Nodes

`cy` currently supports the following layout node types:

- **[Pane](#pane)**: A pane node can be attached to panes that exist in [the node tree](/groups-and-panes.md).
- **[Margins](#margins)**: A margins mode constrains the size of its child node by adding transparent margins.
- **[Split](#split)**: A split node divides the screen space of its parent node in two and provides it to two other nodes, drawing a line on the screen between them.
- **[Borders](#borders)**: A node is enclosed in borders with an optional title on the top or bottom.
- **[Tabs](#tabs)**: A node that can display different pages of content navigable using a familiar tab bar.

The following sections go into these types in more detail.


## Pane

A `:pane` node is an attachment point for a [pane](/groups-and-panes.md#panes).

```janet
{
    :type :pane
    :id nil # number or nil, optional
    :attached true # boolean or nil, optional
    :remove-on-exit false # boolean or nil, optional
}
```

`:id`

Specifies the NodeID this pane is connected to. If `nil`, the part of the screen
occupied by this pane will just show a "disconnected" state.

`:attached`

If `true`, user input will be sent to the pane specified by `:id`.

`:remove-on-exit`

If `true`, when the pane specified by `:id` exits successfully (ie with exit code 0) or is killed using {{api tree/kill}}, this pane will be removed from the layout. This works just like `exit`ing from a shell in `tmux` does: the intent is to preserve that behavior for users who want it.

The value of `:remove-on-exit`, by default, is the value of the [:remove-pane-on-exit](/default-parameters.md#remove-pane-on-exit) parameter.

In other words, to make this global you can do the following:

```janet
(param/set :client :remove-pane-on-exit true)
```

## Margins

{{story static layout/margins}}

A `:margins` node puts transparent margins around its child allowing the current frame to show through.

```janet
{
    :type :margins
    :cols 0 # number, optional
    :rows 0 # number, optional
    :border :rounded # border type, optional
    :border-fg nil # color, optional
    :border-bg nil # color, optional
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

## Split

{{story static layout/split-half}}

A `:split` node divides its visual space in two and gives it to two other nodes, rendering a line down the middle.

```janet
{
    :type :split
    :vertical false # boolean, optional
    :cells nil # int or nil, optional
    :percent nil # int or nil, optional
    :border :rounded # border type, optional
    :border-fg nil # color, optional
    :border-bg nil # color, optional
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

## Borders

{{story static layout/border}}

A `:borders` node surrounds its child in borders and adds an optional title to the top or bottom.

```janet
{
    :type :borders
    :title nil # string or nil, optional
    :title-bottom nil # string or nil, optional
    :border :rounded # border type, optional
    :border-fg nil # color, optional
    :border-bg nil # color, optional
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

## Tabs

{{story static layout/tabs}}

A `:tabs` node is a standard tab system with customizable nibs (these are also sometimes called "leaves").

```janet
{
    :type :tabs
    :active-fg nil # color, optional
    :active-bg nil # color, optional
    :inactive-fg nil # color, optional
    :inactive-bg nil # color, optional
    :bg nil # color, optional
    :bottom false # boolean, optional
    :tabs @[] # list of tabs
}

# tabs look like this:
{
    :active false # boolean, optional
    :name "" # string
    :node {} # a node
}
```

`:active-fg`, `:active-bg`, `:inactive-fg`, and `:inactive-bg`

These are all [colors](/api.md#color) and are used to style the tab's title **if the tab's `:name` property does not contain ANSI escape sequences**, such as those generated by {{api style/render}} et al.

`:bg`

This is the background [color](/api.md#color) for the tab bar.

`:bottom`

If `true`, the tab bar will be on the bottom of the node instead of the top.

### The `:tabs` property

There are some important constraints on the `:tabs` property:

- You must provide at least one tab.
- There must be exactly one tab with `:active` set to `true`.
- All provided tabs must have `:name` fields with non-zero visual width.

### Actions

- {{api action/new-tab}}
- {{api action/set-tab-name}}
- {{api action/next-tab}}
- {{api action/prev-tab}}
