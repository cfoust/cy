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

| Sequence          | Action               | Description                                                     |
| ----------------- | -------------------- | --------------------------------------------------------------- |
| `prefix` `ctrl+p` | `cy/command-palette` | fuzzy-find an action and execute it                             |
| `prefix` `q`      | `cy/kill-server`     | kill the cy server                                              |
| `prefix` `d`      | `cy/detach`          | detach from the cy server (only affects you, not other clients) |
| `prefix` `p`      | `cy/replay`          | start replay mode (akin to `tmux`'s copy mode)                  |
| `prefix` `P`      | `cy/paste`           | paste text copied in replay mode                                |

#### Panes

| Sequence     | Action                 | Description                                                           |
| ------------ | ---------------------- | --------------------------------------------------------------------- |
| `prefix` `j` | `ot/new-shell`         | create a new pane in the same directory as the current pane           |
| `prefix` `n` | `ot/new-project`       | open a project (an `$EDITOR`+`$SHELL` combo) in the current directory |
| `prefix` `k` | `ot/jump-project`      | fuzzy-find a project                                                  |
| `prefix` `l` | `ot/jump-shell`        | fuzzy-find a shell (that was opened with `ot/new-shell`)              |
| `prefix` `;` | `cy/jump-pane`         | fuzzy-find a pane (all of them)                                       |
| `prefix` `x` | `cy/kill-current-pane` | kill the current pane                                                 |

#### Viewport

| Sequence     | Action               | Description                                       |
| ------------ | -------------------- | ------------------------------------------------- |
| `prefix` `g` | `cy/toggle-margins`  | toggle between centering the current pane and not |
| `prefix` `1` | `cy/margins-80`      | set the number of pane columns to 80              |
| `prefix` `2` | `cy/margins-160`     | set the number of pane columns to 160             |
| `prefix` `+` | `cy/margins-smaller` | shrink the margins by 5 columns                   |
| `prefix` `-` | `cy/margins-bigger`  | grow the margins by 5 columns                     |
| `prefix` `r` | `cy/random-frame`    | choose a random frame                             |

### Unprefixed

| Sequence | Action         | Description                          |
| -------- | -------------- | ------------------------------------ |
| `ctrl+l` | `ot/next-pane` | switch to the next pane in the group |

## Fuzzy finding

`(input/find)` has several key bindings that are not yet configurable, but are worth documenting.

| Sequence           | Description                        |
| ------------------ | ---------------------------------- |
| `ctrl+k` or `up`   | move up one option                 |
| `ctrl+j` or `down` | move down one option               |
| `enter`            | choose the option under the cursor |
| `ctrl+c` or `esc`  | quit without choosing              |

## Replay mode

The actions found in the tables below are only valid in a pane that is in replay mode. Replay mode uses an isolated binding scope that can be accessed by providing `:replay` to a `(key/bind)` call:

```janet
(key/bind :replay ["ctrl+b"] (fn [&] (do-something)))
```

### General

| Sequence | Action             | Description                                                                                         |
| -------- | ------------------ | --------------------------------------------------------------------------------------------------- |
| `q`      | `replay/quit`      | quit replay mode                                                                                    |
| `ctrl+c` | `replay/quit`      | "                                                                                                   |
| `esc`    | `replay/quit`      | "                                                                                                   |
| `g` `g`  | `replay/beginning` | go to the beginning of the time range (in time mode) or the first line of the screen (in copy mode) |
| `G`      | `replay/end`       | go to the end of the time range (in time mode) or the last line of the screen (in copy mode)        |

### Time

| Sequence | Action                                    | Description                                                  |
| -------- | ----------------------------------------- | ------------------------------------------------------------ |
| `right`  | `replay/time-step-forward`                | step one event forward in time                               |
| `left`   | `replay/time-step-back`                   | step one event backward in time                              |
| `space`  | `replay/time-play`                        | play or pause playback                                       |
| `1`      | `(fn [&] (replay/time-playback-rate 1))`  | set the playback rate to 1x forwards                         |
| `2`      | `(fn [&] (replay/time-playback-rate 2))`  | set the playback rate to 2x forwards                         |
| `3`      | `(fn [&] (replay/time-playback-rate 5))`  | set the playback rate to 5x forwards                         |
| `!`      | `(fn [&] (replay/time-playback-rate -1))` | set the playback rate to 1x backwards                        |
| `@`      | `(fn [&] (replay/time-playback-rate -2))` | set the playback rate to 2x backwards                        |
| `#`      | `(fn [&] (replay/time-playback-rate -5))` | set the playback rate to 5x backwards                        |
| `/`      | `replay/search-forward`                   | search for a string forwards in time                         |
| `?`      | `replay/search-backward`                  | search for a string backwards in time                        |
| `n`      | `replay/search-again`                     | go to the next match in the direction of the last search     |
| `N`      | `replay/search-reverse`                   | go to the previous match in the direction of the last search |

### Copy mode

Copy mode is entered from time mode by triggering any form of movement, whether that be scrolling or manipulating the cursor.

| Sequence | Action          | Description                               |
| -------- | --------------- | ----------------------------------------- |
| `v`      | `replay/select` | enter visual select mode                  |
| `y`      | `replay/copy`   | yank the selection into the copy buffer |

#### Scrolling

| Sequence | Action                  | Description             |
| -------- | ----------------------- | ----------------------- |
| `up`     | `replay/scroll-up`      | scroll one line up      |
| `down`   | `replay/scroll-down`    | scroll one line down    |
| `ctrl+u` | `replay/half-page-up`   | scroll up half a page   |
| `ctrl+d` | `replay/half-page-down` | scroll down half a page |

#### Movements

Movements in copy mode are intended to be as close to `vim` as possible, but many more will be added over time.

| Sequence        | Action                    | Description                                                                          |
| --------------- | ------------------------- | ------------------------------------------------------------------------------------ |
| `l`             | `replay/cursor-right`     | move right one cell                                                                  |
| `h`             | `replay/cursor-left`      | move left one cell                                                                   |
| `j`             | `replay/cursor-down`      | move down one cell                                                                   |
| `k`             | `replay/cursor-up`        | move up one cell                                                                     |
| `f` `[:re "."]` | `replay/jump-forward`     | jump to the next instance of the provided character on the current line              |
| `F` `[:re "."]` | `replay/jump-backward`    | jump to the previous instance of the provided character on the current line          |
| `t` `[:re "."]` | `replay/jump-to-forward`  | jump to the cell before the provided character after the cursor on the current line  |
| `T` `[:re "."]` | `replay/jump-to-backward` | jump to the cell before the provided character before the cursor on the current line |
| `;`             | `replay/jump-again`       | repeat the last character jump                                                       |
| `,`             | `replay/jump-reverse`     | repeat the inverse of the last character jump                                        |
