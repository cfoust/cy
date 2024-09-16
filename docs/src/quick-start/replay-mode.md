# Entering replay mode

{{story cast cy/replay}}

`cy` records all of your terminal sessions in their entirety. You play them back in an interface called **replay mode**.

[Replay mode](/replay-mode.md) is conceptually similar to `tmux`'s `copy-mode`, but also gives you access to time controls. To open it, type {{bind :root ctrl+a p}}. You can also scroll up with the mouse if the pane's content is scrollable, such as when using a shell.

For basic usage you can use the following keys:

- Step through time using {{bind :time left}} and {{bind :time right}}
- Use {{bind :time space}} to toggle playback
- Go to the beginning of the recording with {{bind :time g g}} and the end with {{bind :time G}}
- Quit with {{bind :time ctrl+c}}

For more information, refer to [the chapter dedicated to replay mode](/replay-mode.md) and [the list of all of its key bindings](/default-keys.md#replay-mode).
