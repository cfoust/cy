# Entering replay mode

{{story cast cy/replay}}

`cy`'s most important feature is that it records all of your terminal sessions in their entirety and lets you jump back in time and replay them at will. It has an interface called **replay mode** that is conceptually similar to `tmux`'s `copy-mode`, but also gives you access to time controls.

You open replay mode for a given pane by typing `ctrl+a` `p`. You can also scroll up with the mouse if the pane's content is scrollable (such as when using a shell.)

Replay mode is powerful and complicated, but for basic usage you can move through time using the left and right arrow keys and hit `space` to play and pause. For more information, refer to [the chapter dedicated to replay mode](./replay-mode.md) and [the list of all of its key bindings](./default-keys.md#replay-mode).

