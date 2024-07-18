# Modes

Like `vim`, replay mode is modal. It has several different modes that influence both what is shown on the screen and what you can do:

- **Time mode:** allows you to pause, play, and move through the history of the current pane back to when it first began.
- **Copy mode:** allows you to explore the state of the screen _at a particular point in time_. This includes the scrollback buffer, which is traditionally all that `tmux`'s copy mode gave you access to.
    - **Visual mode:** This is a submode of copy mode that permits you to select text and copy it into your user-specific copy buffer that can then be pasted elsewhere.

### Time mode

Time mode is similar to a video player: you can pause, play, and skip through time to inspect particular moments in the history of a pane. While playing back terminal events, time mode skips inactivity, here defined as any idle period longer than a second.

#### Searching

Time mode also allows you to search through the pane's history using regular expressions. In this way you can find all instances of a string if it ever appeared on the screen--even if it was subsequently cleared away.

You can initiate a search by hitting `/` to search forward in time and `?` to search backward (by default). Searching supports full regex patterns; you must escape any special characters with `\` if you wish to avoid this behavior.

The search bar also supports time expressions, which you can use to jump by a fixed amount of time. Time expressions are in the format `NdNhNmNs` where `N` is the number of that unit that you wish to move by. You will move in time _in the direction of your search_.

Some examples:

```janet
# ignore
1h30s # one hour 30 seconds
3d # three days
```

### Copy mode

To enter copy mode, all you need to do is invoke any action that would cause the cursor or the viewport to move. Like `tmux`'s copy mode, you can explore the state of the screen and copy text to be pasted elsewhere. Copy mode supports a wide range of cursor and viewport movements that should feel familiar to users of CLI text editors such as `vim`.

#### Visual mode

Visual mode is initiated when you press `v` (by default). It works almost exactly like `vim`'s visual mode does; after you have some selected some text, you can yank it into your buffer with `y` and paste it elsewhere with `ctrl+a` `P`.
