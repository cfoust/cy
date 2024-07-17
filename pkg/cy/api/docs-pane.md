# doc: Attach

(pane/attach pane)

Attach to `pane`, which is a [NodeID](api.md#nodeid) that must correspond to a pane. This is similar to `tmux`'s `attach` command.

# doc: Current

Get the [NodeID](api.md#nodeid) of the current pane.

# doc: Screen

(pane/screen pane)

Get the visible screen lines of the pane referred to by [NodeID](api.md#nodeid). Returns an array of strings.

# doc: HistoryForward

Move forward in the pane history. Works in a similar way to vim's <kbd>ctrl+i</kbd>.

# doc: HistoryBackward

Move backward in the pane history. Works in a similar way to vim's <kbd>ctrl+o</kbd>.
