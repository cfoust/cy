# Replay mode

One of `cy`'s most important features is the ability to record, play back, and search through everything that happens in your terminal sessions. You can invoke **replay mode** at any time by typing the key sequence `ctrl+a` `p` [by default](./default-keys.md#general).

## Modes

Like `vim`, replay mode is modal, meaning that it has several different modes that it can be in that influence both what is shown on the screen and what you can do:

- **Time mode:** allows you to pause, play, and move through the entire history of the current pane back to when it first began.
- **Copy mode:** allows you to explore the state of the screen _at a particular point in time_. This includes the scrollback buffer, which is traditionally all that `tmux`'s copy mode gave you access to.
- **Visual mode:** This is a submode of copy mode that permits you to select text and copy it into your user-specific copy buffer that can then be pasted elsewhere.

### Time mode

When you first initiate replay mode, you are greeted with a screen that looks like this:

### Copy mode

To enter copy mode, all you need to do is invoke any action that would cause the cursor to move.

## Recording terminal sessions to disk

The history of a pane is not only stored in memory; it is also written to a file on your filesystem! This means that you (and only you--`cy` is careful to make sure the directory is only readable by you) can play back any session, even if it is no longer running in a `cy` instance.

By default, `cy` records all of the activity that occurs in a terminal session to `.borg` files, which it stores in one of the following locations:

1.  `$XDG_DATA_HOME/cy`
1.  `$HOME/.local/share/cy`
