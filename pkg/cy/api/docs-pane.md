# doc: Attach

(pane/attach pane)

Attach to `pane`, which is a [NodeID](/api.md#nodeid) that must correspond to a pane. This is similar to `tmux`'s `attach` command.

# doc: Current

Get the [NodeID](/api.md#nodeid) of the current pane. When called from `cy exec`, returns the pane where the command was invoked. When called from a keybinding or action, returns the pane the client is viewing.

# doc: Screen

(pane/screen pane)

Get the visible screen lines of the pane referred to by [NodeID](/api.md#nodeid). Returns an array of strings.

# doc: HistoryForward

Move forward in the pane history. Works in a similar way to vim's <kbd>ctrl+i</kbd>.

# doc: HistoryBackward

Move backward in the pane history. Works in a similar way to vim's <kbd>ctrl+o</kbd>.

# doc: SendKeys

(pane/send-keys pane keys)

Send keys to the pane referred to by [NodeID](/api.md#nodeid). `keys` is an array of strings. Strings that are not [key specifiers](/preset-keys.md) will be written as-is.

```janet
# {
(def pane (cmd/new :root))
# }

# Send ctrl+c to the pane
(pane/send-keys pane @["ctrl+c"])
```

# doc: SendText

(pane/send-text pane text)

Send text to the pane referred to by [NodeID](/api.md#nodeid). Unlike {{api pane/send-keys}}, `text` will not be interpreted as a [key specifier](/preset-keys.md).
