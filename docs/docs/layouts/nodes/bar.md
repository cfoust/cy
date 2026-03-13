---
title: "Bar"
---

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

## Example

The example at the top of the page was created with the following Janet code:

```janet
(def cmd1 (shell/new))
(def cmd2 (shell/new))

(defn
  bar-text
  [[rows cols] layout]
  (def node (layout/attach-id layout))

  (if
    (nil? node) (style/text
                  "detached"
                  :bg "4"
                  :width cols
                  :italic true)
    (style/text
      (tree/path node)
      :bg "5"
      :width cols)))

(layout/set (layout/new
              (bar
                bar-text
                (split
                  (attach :id cmd1)
                  (pane :id cmd2)))))
```
