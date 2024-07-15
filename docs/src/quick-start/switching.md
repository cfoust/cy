# Switching between panes

{{story cast cy/switch-shells}}

`cy` allows you to quickly jump between panes using a built-in fuzzy finder. Try it out by hitting `ctrl+a` `;`, which presents you with a list of all of the running panes.

The controls should be familiar to you if you have ever used a fuzzy finder:

- Typing filters the list.
- Use `ctrl+j` and `ctrl+k` (or the arrow keys) to move up and down.
- Press `enter` to make a selection.
- Quit without making a choice by typing `ctrl+c` or `esc`.

`cy` ships with a few different key bindings for choosing a pane:

- `ctrl+a` `k`: Jump to a project.
- `ctrl+a` `l`: Jump to a shell based on its current working directory.
