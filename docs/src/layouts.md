# Layouts

`cy` uses a declarative, tree-based system for defining how panes are displayed on the screen. It refers to this as the **layout**. A layout is just a configuration for how the `cy` screen should look. It is composed of **nodes** of different types, like panes, splits, et cetera. In other words, a layout is a description of all of the visual elements on the screen.

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

A node consists of a Janet struct with a `:type` property indicating the type of the node and a set of properties that describe how the node should appear. The layout above describes a horizontal split (two nodes side by side). The client is attached to the left pane.

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

Every node type has a set of properties that determines that node's appearance and functionality. Here are some common examples:

* The color and style of that node's borders
* The children of that node, one or more of which may be shown at any one time
* Titles shown above or below the node

Valid values for properties are usually primitive types in Janet, such as strings, numbers, and booleans. But most node types also have **dynamic properties**, which are properties whose values are computed _when that node is rendered_. Instead of providing a static value for a property, you instead provide a function that returns a value of the property's original type.

## Layout actions

You can access some layout functionality using [actions](/keybindings.md#actions) that are available in the command palette:

- **{{api action/add-node}}**: Quickly add nodes of any type to the layout.
- **{{api action/remove-parent}}**: Remove the most recent parent of the currently attached node.
- **{{api action/clear-layout}}**: Completely empty out the layout.

Some node types have actions as well. Each node type in the next chapter describes those actions, where appropriate.

## Frames

The patterned background seen in the screenshot above is referred to as the **frame**. `cy` comes with a [range of different frames](/frames.md). You can choose between all of the available frames using the {{api action/choose-frame}} function, which is bound by default to {{bind :root ctrl+a F}}, and set the default frame on startup using the [`:default-frame`](/default-parameters.md#default-frame) parameter.
