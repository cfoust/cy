# Creating a project

{{story cast cy/project}}

Often, you will be doing some work in a single directory, like a Git repository. `cy` comes with a way to create a new group of panes for exactly this purpose. To use it, navigate to a directory and type {{bind :root ctrl+a n}}.

This creates two panes:

1. `/projects/[base-name]/editor`: A pane running the program specified by the `$EDITOR` environment variable.
1. `/projects/[base-name]/shell`: A pane running your default shell (or the value of `$SHELL`).

`[base-name]` is the basename (a la the Bash `basename` command) of the directory in which you opened the project.

For example, if you type {{bind :root ctrl+a n}} while in a pane with the working directory `/tmp/test-dir`, `[base-name]` would be `test-dir` and the node tree would have the following structure:

```
/ (group)
├── /shells (group)
│   └── [...]
├── /projects (group)
│   └── /test-dir (group)
│       ├── /editor (pane) <- attached here
│       └── /shell (pane)
└── /logs (pane)
```

