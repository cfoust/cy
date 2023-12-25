# Quick start

This guide introduces `cy`'s basic concepts in a step-by-step tutorial.

## 1. Starting `cy`

To start `cy` after installation, just run `cy` without any arguments. `cy` will connect to the `cy` server, starting one if necessary.

{{story png splash}}

`cy` shows a splash screen on startup that looks like this. To clear it, press any key.

## 2. Using the viewport

When you first connect, `cy` creates a new **pane** and attaches to it. By default, it runs your user's default shell.

If your terminal is wider than 80 columns, you'll notice that the pane does not occupy the full width of the screen; `cy` centers the pane and fills the rest of the horizontal space with a patterned background.

{{story png placeholder}}

`cy` refers to this as **the viewport**.

All actions in `cy`, such as creating panes and switching between them, are triggered by sequences of keys.

Here are a few you can try:

1. To make the pane fill the entire viewport, type `ctrl+a` `g`. (Repeat to center it again.)
1. To set the width of the pane to 80 columns, type `ctrl+a` `1`.
1. To set it to 160 columns, type `ctrl+a` `2`.
1. To increase the width by 5 columns, type `ctrl+a` `+`.
1. To decrease the width by 5 columns, type `ctrl+a` `-`.

## 3. Creating a new shell

To create a new pane, type `ctrl+a` `j`. This creates a shell in the current directory. You can return to the old one with `ctrl+l`; this cycles between all of the panes in the current **group**. In `cy`, a group is just a container for panes or other groups.

Every **group** and **pane** in `cy` has a path, just like a file in a filesystem. The new shell you created has a path like `/shells/3`.

The collection of all **groups** and **panes** is referred to as **the node tree**. When you first start `cy`, the node tree looks like this:

```
/ (group)
├── /shells (group)
│   └── /2 (pane) <- you start here
└── /logs (pane)
```

And here's how it looks after you create a new shell with `ctrl+a` `j`:

```
/ (group)
├── /shells (group)
│   ├── /2 (pane)
│   └── /3 (pane) <- now you're here
└── /logs (pane)
```

When you are attached to a pane in the `/shells` group, `ctrl+l` cycles to the next sibling pane.

## 4. Creating a new project

Often, you will be doing some work in a single directory, like a Git repository. `cy` ships with a way to create a new group of panes for exactly this purpose. To use it, navigate to a directory and type `ctrl+a` `n`.

This creates two panes:

1. `/projects/[base-name]/editor`: A pane running the program specified by the `$EDITOR` environment variable.
1. `/projects/[base-name]/shell`: A pane running your default shell (or the value of `$SHELL`).

`[base-name]` is the basename (a la the Bash `basename` command) of the directory in which you opened the project. For example, if you type `ctrl+a` `n` while in a pane with the working directory `/tmp/test-dir`, `[base-name]` would be `test-dir`.
