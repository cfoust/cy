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

## Replay mode

The actions found in the tables below are only accessible in a pane that is in replay mode. Replay mode uses an isolated binding scope that can be accessed by providing `:replay` to a `(key/bind)` call:

```janet
(key/bind :replay ["ctrl+b"] (fn [&] (do-something)))
```

### General

| Sequence | Action             | Description |
| -------- | ------------------ | ----------- |
| `q`      | `replay/quit`      |             |
| `ctrl+c` | `replay/quit`      |             |
| `esc`    | `replay/quit`      |             |
| `g` `g`  | `replay/beginning` |             |
| `G`      | `replay/end`       |             |

### Time

| Sequence | Action                                    | Description |
| -------- | ----------------------------------------- | ----------- |
| `right`  | `replay/time-step-forward`                |             |
| `left`   | `replay/time-step-back`                   |             |
| `space`  | `replay/time-play`                        |             |
| `1`      | `(fn [&] (replay/time-playback-rate 1))`  |             |
| `2`      | `(fn [&] (replay/time-playback-rate 2))`  |             |
| `3`      | `(fn [&] (replay/time-playback-rate 5))`  |             |
| `!`      | `(fn [&] (replay/time-playback-rate -1))` |             |
| `@`      | `(fn [&] (replay/time-playback-rate -2))` |             |
| `#`      | `(fn [&] (replay/time-playback-rate -5))` |             |
| `/`      | `replay/search-forward`                   |             |
| `?`      | `replay/search-backward`                  |             |
| `n`      | `replay/search-again`                     |             |
| `N`      | `replay/search-reverse`                   |             |

### Copy mode

| Sequence | Action          | Description |
| -------- | --------------- | ----------- |
| `v`      | `replay/select` |             |
| `y`      | `replay/copy`   |             |

#### Scrolling

| Sequence | Action                  | Description |
| -------- | ----------------------- | ----------- |
| `up`     | `replay/scroll-up`      |             |
| `down`   | `replay/scroll-down`    |             |
| `ctrl+u` | `replay/half-page-up`   |             |
| `ctrl+d` | `replay/half-page-down` |             |

#### Movements

| Sequence        | Action                    | Description |
| --------------- | ------------------------- | ----------- |
| `l`             | `replay/cursor-right`     |             |
| `h`             | `replay/cursor-left`      |             |
| `j`             | `replay/cursor-down`      |             |
| `k`             | `replay/cursor-up`        |             |
| `;`             | `replay/jump-again`       |             |
| `,`             | `replay/jump-reverse`     |             |
| `f` `[:re "."]` | `replay/jump-forward`     |             |
| `F` `[:re "."]` | `replay/jump-backward`    |             |
| `t` `[:re "."]` | `replay/jump-to-forward`  |             |
| `T` `[:re "."]` | `replay/jump-to-backward` |             |
