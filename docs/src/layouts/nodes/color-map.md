# Color map

{{story cast layout/dynamic/color-map}}

A `:color-map` node applies a [color map](/parameters/colors.md#color-maps) to its contents.

```janet
{
    :type :color-map
    :map @{} # color map, dynamic
    :node {} # a node
}
```

`:map`

A [color map](/api.md#color-map).

`:node`

A valid layout node.


## Example

The example at the top of the page was created with the following Janet code:

```janet
(def cmd1 (cmd/new :root :command "htop"))
(def cmd2 (cmd/new :root :command "htop"))
(def cmd3 (cmd/new :root :command "htop"))
(defn
  theme [layout]
  (def node (layout/attach-id layout))
  (if
    # Apply one color scheme if the client is not attached to this node
    (nil? node) ((color-maps/get :atelier-sulphurpool) :map)
    # And a different one if they are
    ((color-maps/get :atelier-sulphurpool-light) :map)))

(layout/set (layout/new
              (split
	        (split
		  (color-map
		    theme
		    (attach :id cmd1))
		  (color-map
		    theme
		    (pane :id cmd2))
		  :vertical true)
                (color-map
                  theme
                  (pane :id cmd3)))))
```
