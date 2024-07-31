# Why not tmux?

{{story cast fluid-fuzzy --height 20 --width 120}}

> _`tmux` is just not weird enough._

`cy` shares some basic similarities with `tmux`. For example, it runs as a daemon, so its state is preserved even if you disconnect. But for `cy` to be a compelling alternative, it has to do more than just mimic `tmux`'s functionality and be written in a fashionable systems programming language.

I started writing `cy` because I felt like terminal multiplexers had stagnated. There have been some recent attempts to modernize the concept, but I didn't feel like they went far enough.

`cy` improves on `tmux` in three main ways:

1. **Session playback**: `cy` records your terminal sessions and lets you play back and search through them.
1. **Interface**: `cy` has a simple layout designed for use on large screens.
1. **Configuration**: `cy` uses a real programming language, [Janet](https://janet-lang.org/), for configuration.

## Session playback

If you use `tmux`, you might be familiar with `copy-mode`, which allows you to view lines from [the scrollback buffer](https://unix.stackexchange.com/q/145050). This is nice when you're using a program like `bash`, where you issue commands that produce static output.

But by definition, `copy-mode` ceases to be useful when you use a program with any interactivity, such as `vim`: you only see the last lines it left on the screen, and certainly cannot see what the screen used to look like.

`cy` solves this problem by recording all of your terminal sessions. In [replay mode](/replay-mode.md) you can seek, play back, and search through the history of a pane--regardless of the application that was running in it. `cy` [saves these recordings to disk](/replay-mode.md#recording-terminal-sessions-to-disk) and allows you to open them later at your leisure.

> _This sounds like it would be a lot, but it isn't: after six months and hundreds of hours of work in the terminal, all of my recordings occupy a total of 100MB (!) on disk._

Without trying it for yourself, it's hard to appreciate just how useful it is to be able to go back in time to replay everything you've seen or done in a terminal session. `cy` aims to augment your memory in a way that other programs cannot.

## Interface

{{story png placeholder}}

`cy` has a ridiculously flexible [layout system](/layouts.md) and the state of all of the panes you have running are separate from what is actually shown on the screen, just like buffers in `vim`.

Most of the time, however, you can keep things simple, because `cy` makes it easy to switch between panes using [fuzzy finding](/user-input/fuzzy-finding.md) (with previews!). It also contains a minimal, [filesystem-like abstraction](/groups-and-panes.md) for grouping panes together.

As of writing, `cy` also lacks `tmux`'s status line, but that should change sooner or later.

## Configuration

Anyone who has tried to do anything sophisticated with `tmux` runs into a familiar set of problems:

1. `tmux`'s configuration language is hacky and primitive, which makes it hard to do anything interesting without running an external command.
2. Its key binding system is limited.

`cy` allows you to bind arbitrary sequences of keys to [Janet](https://janet-lang.org/) functions. It even supports binding [regexes](/keybindings.md#regexes), matches for which will be passed to the function you provide. You can also create bindings and settings that are in effect only when attached to a [specific pane or group of panes](/groups-and-panes.md#groups).
