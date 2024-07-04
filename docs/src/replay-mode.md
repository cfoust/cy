# Replay mode

{{story cast cy/replay}}

One of `cy`'s most important features is the ability to record, play back, and search through everything that happens in your terminal sessions. You can invoke **replay mode** at any time by typing the key sequence `ctrl+a` `p` [by default](./default-keys.md#general) or by scrolling up with the mouse if the pane is connected to a shell.

### A note about recording

`cy` does not and will never record what you type (otherwise known as "standard in" or `stdin`). It only records the output of the process (otherwise known as "standard out" or `stdout`) that is attached to your virtual terminal and nothing more.

This is a basic safety measure so that your passwords (such as for `sudo`) never appear in `cy`'s recordings. Your secrets (such as authentication keys and tokens) still will if they ever appear on your screen, so caution is advised.

In the future, `cy` may give you more fine-grained control over specifically what it records and when, but for now this is not configurable.

If you wish to opt out of recording to disk entirely, set [the `:data-dir` parameter](./parameters.md#default-parameters) to an empty string. Note that `cy` will continue to hold on to your terminal sessions in memory.

## Modes

Like `vim`, replay mode is modal, meaning that it has several different modes that it can be in that influence both what is shown on the screen and what you can do:

- **Time mode:** allows you to pause, play, and move through the entire history of the current pane back to when it first began.
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
1h30s # one hour 30 seconds
3d # three days
```

### Copy mode

To enter copy mode, all you need to do is invoke any action that would cause the cursor or the viewport to move. Like `tmux`'s copy mode, you can explore the state of the screen and copy text to be pasted elsewhere. Copy mode supports a wide range of cursor and viewport movements that should feel familiar to users of CLI text editors such as `vim`.

#### Visual mode

Visual mode is initiated when you press `v` (by default). It works almost exactly like `vim`'s visual mode does; after you have some selected some text, you can yank it into your buffer with `y` and paste it elsewhere with `ctrl+a` `P`.

## Recording terminal sessions to disk

The history of a pane is not only stored in memory; it is also written to a file on your filesystem. This means that you (and only you--`cy` is careful to make sure the directory is only readable by you) can play back any session, even if it is no longer running in a `cy` instance.

By default, `cy` records all of the activity that occurs in a terminal session to `.borg` files, which it stores in one of the following locations:

1.  `$XDG_DATA_HOME/cy` (if `$XDG_DATA_HOME` is set)
1.  `$HOME/.local/share/cy` (if it's not)

The directory will be created if it does not exist.

You can access previous sessions through the {{api action/open-log}} action, which by default can be invoked by searching for `Open a .borg file.` in the command palette (`ctrl+a` `ctrl+p`).

You are also free to use the API call {{api replay/open}} to open `.borg` files anywhere on your filesystem.
