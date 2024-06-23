# Default key bindings

All of `cy`'s default key bindings use [actions](./keybindings.md#actions) defined in the global scope and therefore are easy to rebind should you so desire. For example, to assign `cy/command-palette` to another key sequence:

```janet
(key/bind :root ["ctrl+b" "p"] cy/command-palette)
```

## Global

These bindings apply everywhere and can always be invoked.

### Prefixed

All of the bindings in this table are prefixed by `ctrl+a` by default. This currently cannot be changed without rebinding all of the keys individually.

#### General

{{keys root general --skip 1}}

#### Panes

{{keys root panes --skip 1}}

#### Viewport

{{keys root viewport --skip 1}}

### Unprefixed

These bindings are not prefixed by `ctrl+a`.

{{keys root unprefixed}}

## Fuzzy finding

[`(input/find)`](api.md#inputfind) has several key bindings that are not yet configurable, but are worth documenting.

| Sequence                             | Description                         |
| ------------------------------------ | ----------------------------------- |
| <kbd>ctrl+k</kbd> or <kbd>up</kbd>   | Move up one option.                 |
| <kbd>ctrl+j</kbd> or <kbd>down</kbd> | Move down one option.               |
| <kbd>enter</kbd>                     | Choose the option under the cursor. |
| <kbd>ctrl+c</kbd> or <kbd>esc</kbd>  | Quit without choosing.              |

## Replay mode

The actions found in the tables below are only valid in a pane that is in replay mode. Replay mode uses two isolated binding scopes that can be accessed by providing `:time` (for time mode) or `:copy` (for copy mode) to a [`(key/bind)`](api.md#keybind) call:

```janet
(key/bind :time ["ctrl+b"] (fn [&] (do-something)))
```

### Time mode

{{keys time general}}

### Copy mode

Copy mode is entered from time mode by triggering any form of movement, whether that be scrolling or manipulating the cursor.

{{keys copy general}}

#### Movements

Movements in copy mode are intended to be as close to `vim` as possible, but many more will be added over time.

{{keys copy motion}}
