# Replay mode

{{story cast cy/replay}}

One of `cy`'s main features is the ability to record, play back, and search through everything that happens in your terminal sessions. You can invoke **replay mode** at any time by typing the key sequence {{bind :root ctrl+a p}} [by default](./default-keys.md#general) or by scrolling up with the mouse if the pane is connected to a shell.

## Recording to disk

The history of a pane is not only stored in memory; it is also written to a file on your filesystem. This means that you (and only you--`cy` is careful to make sure the directory is only readable by you) can play back any session, even if it is no longer running in a `cy` instance.

By default, `cy` records all of the activity that occurs in a terminal session to `.borg` files, which it stores in one of the following locations:

1.  `$XDG_DATA_HOME/cy` (if `$XDG_DATA_HOME` is set)
1.  `$HOME/.local/share/cy` (if it's not)

The directory will be created if it does not exist.

You can access previous sessions through the {{api action/open-log}} action, which by default can be invoked by searching for `Open a .borg file.` in the command palette ({{bind :root ctrl+a ctrl+p}}).

You are also free to use the API function {{api replay/open-file}} to open `.borg` files anywhere on your filesystem.

## A warning about recording

`cy` does not and will never record what you type (otherwise known as "standard input" or `stdin`). It only records the output of the process (otherwise known as "standard output" or `stdout`) that is attached to your virtual terminal and nothing more.

This is a basic safety measure so that your passwords (such as for `sudo`) never appear in `cy`'s recordings. Your secrets (such as authentication keys and tokens) still will if they ever appear on your screen, so caution is advised.

In the future, `cy` may give you more fine-grained control over specifically what it records and when, but for now this is not configurable.

If you wish to opt out of recording to disk entirely, set [the `:data-directory` parameter](./parameters.md#default-parameters) to an empty string. Note that `cy` will continue to hold on to your terminal sessions in memory.

For example:

```janet
(param/set :root :data-directory "")
```
