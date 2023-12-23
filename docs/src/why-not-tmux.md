# Why not tmux?

In order for `cy` to be a compelling alternative to `tmux`, it has to do more than just be written in a fashionable systems programming language.

`cy` improves on `tmux` in three main ways:

1. **Session playback**: `cy` records your terminal sessions and lets you play back and search through them.
1. **Interface**: `cy` has a simple, flexible layout designed for use on large screens.
1. **Configuration**: `cy` uses a real programming language, [Janet](https://janet-lang.org/), for configuration.

## Session playback

If you use `tmux`, you might be familiar with `copy-mode`, which allows you to view lines from [the scrollback buffer](https://unix.stackexchange.com/q/145050). This is nice when you're using a program like `bash`, where you issue commands that produce output. Yet by definition, `copy-mode` ceases to work when you use a program with any interactivity.

`cy` takes this even further. By default, it records all of your terminal sessions, just like a screen recording. It has a mode called [replay mode](./replay-mode.md) in which you can seek, play back, and search through the history of a pane--regardless of the application that was running in it. `cy` [saves these recordings](replay-mode.md#recording-terminal-sessions-to-disk) and allows you to open them later at your leisure.

## Interface

`cy` does not have windows and panes like `tmux` does. It displays one terminal session at a time, and by default that session is centered on your screen with a fixed number of columns. This lets you concentrate on one "pane" at a time.

To some, this may seem like a surprising decision. The predominant abstraction for terminal multiplexers for at least a couple of decades has been the familiar pattern of vertical and horizontal splits. Yet in my experience, particularly as someone who does all of their work in the terminal, I found that I spent more time fighting with this (or writing [plugins to remove it](https://github.com/cfoust/tmux-oakthree)) than benefiting from it. Rarely do I feel like I need to see more than one pane at a time.

This is practical because `cy` makes it easy to switch between panes. It also contains a sensible, [filesystem-like abstraction](./groups-and-panes.md) for grouping panes together.

As of writing, `cy` also lacks `tmux`'s status line. Because there is no notion of windows, there is nothing akin to the tab-like behavior that `tmux` encourages (and thus, little immediate need to display it.)

It is worth stating that neither of these are principled omissions. My intent with `cy` is to, sooner or later, address these use cases in a flexible, configurable way.

## Configuration

`tmux` uses a strange quasi-programming language for configuration, which makes it hard to do anything very sophisticated without running an external command. `cy` uses [Janet](https://janet-lang.org/).
