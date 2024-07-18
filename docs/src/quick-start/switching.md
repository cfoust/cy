# Switching panes

{{story cast cy/switch-shells}}

`cy` allows you to quickly jump between panes using a built-in fuzzy finder. Try it out by hitting {{bind :root ctrl+a ;}}, which presents you with a list of all of the running panes.

The controls should be familiar to you if you have ever used a fuzzy finder:

- Typing filters the list.
- Use <kbd>ctrl+j</kbd> and <kbd>ctrl+k</kbd> (or the arrow keys) to move up and down.
- Press <kbd>enter</kbd> to make a selection.
- Quit without making a choice by typing <kbd>ctrl+c</kbd> or <kbd>esc</kbd>.

`cy` ships with a few different key bindings for choosing a pane:

- {{bind :root ctrl+a k}}: Jump to a project.
- {{bind :root ctrl+a l}}: Jump to a shell based on its current working directory.
