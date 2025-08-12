# Pane

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

If `true`, when the pane specified by `:id` exits successfully (ie with exit code 0) or is killed using {{api tree/rm}}, this pane will be removed from the layout. This works just like `exit`ing from a shell in `tmux` does: the intent is to preserve that behavior for users who want it.

The value of `:remove-on-exit`, by default, is the value of {{param remove-pane-on-exit}}.

In other words, to make this global you can do the following:

```janet
(param/set :client :remove-pane-on-exit true)
```
