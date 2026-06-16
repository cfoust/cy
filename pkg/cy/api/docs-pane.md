# doc: Attach

(pane/attach pane)

Attach to `pane`, which is a [NodeID](/api.md#nodeid) that must correspond to a pane. This is similar to `tmux`'s `attach` command.

# doc: Current

Get the [NodeID](/api.md#nodeid) of the current pane. When called from `cy exec`, returns the pane where the command was invoked. When called from a keybinding or action, returns the pane the client is viewing.

# doc: Screen

(pane/screen pane)

Get a snapshot of the visible grid of the pane referred to by [NodeID](/api.md#nodeid). This is what is displayed on screen right now; for the full logical history, use {{api pane/scrollback}}.

Returns a struct with:
- `:lines` (array of strings): The visible screen lines, one per row of the terminal.
- `:is-alt` (boolean): Whether the terminal is in alternate screen mode (e.g., when running vim, htop, or other fullscreen applications).

```janet
# {
(def pane (cmd/new :root))
# }

(def result (pane/screen pane))
(print (result :lines))
(print (result :is-alt))
```

# doc: Scrollback

(pane/scrollback pane)

Get the full logical history of the pane referred to by [NodeID](/api.md#nodeid): the scrollback buffer plus the current screen, reflowed at the pane's current width. Returns an array of strings.

Unlike {{api pane/screen}}, blank padding rows are collapsed and there is no alternate-screen indicator. This is the right call for capturing the complete output of a command, while `pane/screen` is better for snapshotting a fullscreen application.

```janet
# {
(def pane (cmd/new :root))
# }

(def lines (pane/scrollback pane))
(print (string/join lines "\n"))
```

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

# doc: SendBytes

(pane/send-bytes pane data)

Send raw bytes to the pane referred to by [NodeID](/api.md#nodeid). Unlike {{api pane/send-text}}, `data` is written directly to the pane's underlying stream without bracketed paste wrapping, so escape sequences will be interpreted by the shell.

```janet
# {
(def pane (cmd/new :root))
# }

# Send an escape sequence to the pane
(pane/send-bytes pane "\x1b[1;6y")
```

# doc: SendText

(pane/send-text pane text)

Send text to the pane referred to by [NodeID](/api.md#nodeid). Unlike {{api pane/send-keys}}, `text` will not be interpreted as a [key specifier](/preset-keys.md).
