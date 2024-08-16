# Layouts

`cy` uses a declarative, tree-based system for defining what is shown on the screen. It refers to this as the **layout**. It is composed of **nodes** of different types such as panes, splits, and tabs.

The structure of a typical layout might look something like this:

```janet
{
    :type :split
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

Every node consists of a Janet struct with a `:type` property indicating the type of the node and a set of properties that describe how the node should look and behave. The layout above describes a horizontal split (two nodes side by side). The client is attached to the left pane.

A layout must have exactly one pane node where `:attached` is `true`. The attached node is the visual element to which the client's input (that does not match any bindings) is sent.

## API

Layouts are just standard Janet values. You can retrieve the current layout with {{api layout/get}} and set it with {{api layout/set}}.

`cy` includes a comprehensive [Janet library](https://github.com/cfoust/cy/blob/main/pkg/cy/boot/layout.janet) with handy tools for manipulating them more easily. For example, you can quickly create nodes with a family of creation functions like {{api layout/pane}}, {{api layout/split}}, {{api layout/vsplit}}, et cetera.

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

## Properties

Every node type has a set of properties that determines that node's appearance and functionality. Valid values for properties are usually primitive types in Janet, such as strings, numbers, and booleans. Here are some common examples:

- The color and style of that node's borders
- The children of that node, one or more of which may be shown at any one time
- Titles shown above or below the node

But most node types also have **dynamic properties**. In addition to accepting static values, dynamic properties can be set to a function that returns a value of the proper type. That function is executed _when that node is rendered_. The arguments to that function vary based on the node and the property.

To illustrate, [border nodes](/layouts/nodes.md#borders) have a property called `:title`, which is a string that is shown at the top-right of the node. The `:border-fg` property determines the color of the border surrounding the node.

By providing a function to both properties, we can change the color and title of the borders node as the user switches between panes. In this case, the functions are invoked with a single argument: the value of the `:node` property in each borders node, which contains whatever node is displayed inside of the borders.

This lets you accomplish things like this:

{{story cast layout/dynamic/borders}}

This example uses the following code:

```janet
(def cmd1 (shell/new))
(def cmd2 (shell/new))

(defn
  border-title
  [layout]
  # Get the NodeID of the node the user is attached to
  (def node (layout/attach-id layout))
  (if
    (nil? node) (style/text "detached" :bg "4" :italic true)
    (style/text (tree/path node) :bg "5")))

(defn
  border-fg
  [layout]
  (def node (layout/attach-id layout))

  (if (nil? node) "4" "5"))

(layout/set (layout/new
              (split
                (borders
                  (attach :id cmd1)
                  :title border-title
                  :border-fg border-fg)
                (borders
                  (pane :id cmd2)
                  :title border-title
                  :border-fg border-fg))))
```

Swapping panes causes the layout to change by changing the pane where `:attached` is `true`. This causes the layout to rerender, which reruns the functions we provided when we created the borders node. Since now the `:node` that each borders node renders has changed, the `:title` and `:border-fg` properties produce different results, all without explicitly calling {{api layout/set}} again.

## Actions

You can access some layout functionality using [actions](/keybindings.md#actions) that are available in the command palette:

- **{{api action/add-node}}**: Quickly add nodes of any type to the layout.
- **{{api action/remove-parent}}**: Remove the most recent parent of the currently attached node.
- **{{api action/clear-layout}}**: Completely empty out the layout.

Some node types have actions as well. Each node type in the next chapter describes those actions, where appropriate.

## Frames

The patterned background seen in the screenshot above is referred to as the **frame**. `cy` comes with a [range of different frames](/frames.md). You can choose between all of the available frames using the {{api action/choose-frame}} function, which is bound by default to {{bind :root ctrl+a F}}, and set the default frame on startup using the [`:default-frame`](/default-parameters.md#default-frame) parameter.
