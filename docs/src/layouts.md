# Layouts

`cy` uses a declarative, tree-based system for defining how panes are displayed on the screen. Every `cy` client has a layout that can be retrieved with {{api layout/get}} and set with {{api layout/set}}.

The structure of a typical layout returned by {{api layout/get}} might look something like this:

```janet
{
    :type :split
    :percent 26
    :border :none
    :vertical false
    :a {
        :type :pane
        :id 1
        :attached true
    }
    :b {
        :type :pane
        :id 2
    }
}
```

A layout is just a configuration for how the `cy` screen should look. It contains a description of all of the different visual elements on the screen including their locations, settings, and appearances.

A layout is composed of nodes of different types. It need only adhere to a single constraint: it must have exactly one pane node where `:attached` is `true`. The attached node is the visual element to which the client's user input (that does not match any bindings) is sent.

## Programmatic use

Layouts are just standard Janet values. `cy` comes with a comprehensive [Janet library](https://github.com/cfoust/cy/blob/main/pkg/cy/boot/layout.janet) with handy tools for manipulating them more easily. For example, you can quickly create nodes with a family of creation functions like {{api layout/pane}}, {{api layout/split}}, {{api layout/vsplit}}, et cetera.

There is also {{api layout/new}}, which is a Janet macro that expands shortened forms of these node creation functions into their full forms for you. This lets you describe layouts succinctly:

```janet
(layout/new (split
              (pane)
              (vsplit
                (margins (attach))
                (borders (pane)))))

# Expands to:
{:type :split
 :vertical false
 :a {:attached false :type :pane}
 :b {:type :split
     :vertical true
     :a {:type :margins
         :node {:attached true :type :pane}}
     :b {:type :borders
         :node {:attached false :type :pane}}}}
```

## Creating layouts at runtime

You can access most layout functionality without ever writing a line of code using actions that are available in the command palette:

- **{{api action/add-node}}**: Quickly add nodes of any type to the layout.
- **{{api action/remove-parent}}**: Remove the most recent parent of the currently attached node.
- **{{api action/clear-layout}}**: Completely empty out the layout.

Some node types have actions as well. For example, the `:borders` type has {{api action/set-borders-title}} and {{api action/set-borders-title-bottom}}.

## Node types

`cy` currently supports the following layout node types:

- **[Pane](#pane)**: A pane node can be attached to panes that exist in [the node tree](/groups-and-panes.md).
- **[Margins](#margins)**: A margins mode constrains the size of its child node by adding transparent margins.
- **[Split](#split)**: A split node divides the screen space of its parent node in two and provides it to two other nodes, drawing a line on the screen between them.
- **[Borders](#borders)**: A node is enclosed in borders with an optional title on the top or bottom.

The following sections go into these types in more detail.

### Pane

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

### Margins

{{story cast layout/margins}}

A `:margins` node puts transparent margins around its child allowing the current frame to show through.

```janet
{
    :type :margins
    :cols 0 # number, optional
    :rows 0 # number, optional
    :border :rounded # border type, optional
    :node {} # a node
}
```

`:cols` and `:rows`

These properties set the size of the node inside of the `:margins` node; they do not refer to the width of the margins. A value of 0 for either property means that the node will use the full space available to it in that dimension.

`:border`

The [border style](#border-styles) for the borders around the node.

`:node`

A valid layout node.

### Split

{{story cast layout/split-half}}

A `:split` node divides its visual space in two and gives it to two other nodes, rendering a line down the middle.

```janet
{
    :type :split
    :vertical false # boolean, optional
    :cells nil # int or nil, optional
    :percent nil # int or nil, optional
    :border :rounded # border type, optional
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

`:a` and `:b`

Both must be valid layout nodes.

### Borders

{{story cast layout/border}}

A `:borders` node surrounds its child in borders and adds an optional title to the top or bottom.

```janet
{
    :type :borders
    :title nil # string or nil, optional
    :title-bottom nil # string or nil, optional
    :border :rounded # border type, optional
    :node {} # a node
}
```

`:title` and `:title-bottom`

These strings will be rendered on the top and the bottom of the window, respectively.

`:border`

The [border style](#border-styles) for this node. `:none` is not supported.

`:node`

A valid layout node.

## Border styles

Some nodes have a `:border` property that determines the appearance of the node's borders. The border property can be one of the following keywords:

- `:normal`
- `:rounded`
- `:block`
- `:outer-half`
- `:inner-half`
- `:thick`
- `:double`
- `:hidden`
- `:none`

`:hidden` still renders the border with blank cells; `:none` does not render a border at all.

### Frames

The patterned background seen in the screenshot above is referred to as the **frame**. `cy` comes with a [range of different frames](/frames.md). You can choose between all of the available frames using the {{api action/choose-frame}} function, which is bound by default to {{bind :root ctrl+a F}}, and set the default frame on startup using the [`:default-frame`](/default-parameters.md#default-frame) parameter.

### Styling

{{story cast layout/styled}}

All string layout properties accept text styled with {{api style/render}} (or {{api style/text}}).

The layout shown in the asciicast above was generated with the following code:
```janet
(def cmd1 (shell/new))
(def cmd2 (shell/new))
(layout/set
	(layout/new
		(split
		  (borders
		    (attach :id cmd1)
		    :title (style/text "some pane" :bg "6")
		    :title-bottom (style/text "some subtitle" :bg "6"))
		  (borders
		    (pane :id cmd2)
		    :title (style/text "some pane" :italic true :bg "5")
		    :title-bottom (style/text "some subtitle" :italic true :bg "5"))
		  :border :none)))
```
