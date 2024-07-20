# Modes

Like `vim`, replay mode is modal. It has several different modes that influence both what is shown on the screen and what you can do:

- **Time mode:** allows you to pause, play, and move through the history of the current pane back to when it first began.
- **Copy mode:** allows you to explore the state of the screen _at a particular point in time_. This includes the scrollback buffer, which is traditionally all that `tmux`'s copy mode gave you access to.
  - **Visual mode:** This is a submode of copy mode that permits you to select text and copy it into your user-specific copy buffer that can then be pasted elsewhere.

### Time mode

{{story cast replay/time-demo}}

Time mode is similar to a video player: you can pause, play (both forwards and backwards!), and skip through time to inspect particular moments in the history of a pane. While playing back terminal events, time mode skips inactivity, here defined as any idle period longer than a second.

#### Searching

{{story cast replay/time-demo-search}}

Time mode also allows you to search through the pane's history using regular expressions. In this way you can find all instances of a string if it ever appeared on the screen--even if it was subsequently cleared away. Note that _this is different from searching in the scrollback._ Searching in time mode will find matches that appeared on the screen at any point in time, including in full-screen applications such as `vim` or `htop`.

You can initiate a search by hitting {{bind :time /}} to search forward in time and {{bind :time ?}} to search backward (by default). Searching supports full regex patterns; you must escape any special characters with `\` if you wish to avoid this behavior.

The search bar also supports time expressions, which you can use to jump by a fixed amount of time. Time expressions are in the format `NdNhNmNs` where `N` is the number of that unit that you wish to move by. You will move in time _in the direction of your search_.

{{story cast replay/time-demo-search-time}}

Some examples:

```janet
# ignore
1h30s # one hour 30 seconds
3d # three days
```

### Copy mode

To enter copy mode, all you need to do is invoke any action that would cause the cursor or the viewport to move. Like `tmux`'s copy mode, you can explore the state of the screen and copy text to be pasted elsewhere. Copy mode supports a wide range of cursor and viewport movements that should feel familiar to users of CLI text editors such as `vim`. For a full list of supported motions, refer to the [reference page for key bindings](../default-keys.md#movements).

Copy mode also allows you to swap between the terminal's main and alt screens using {{bind :copy s}}. In other words, even if you run a full-screen application such as `htop`, you can still swap back to the scrollback buffer and see the output of commands you ran before running `htop`.

#### Visual mode

Visual mode is initiated when you press {{bind :copy v}} (by default). It works almost exactly like `vim`'s visual mode does; after you have some selected some text, you can yank it into your buffer with {{bind :copy y}} and paste it elsewhere with {{bind :root ctrl+a P}}.
