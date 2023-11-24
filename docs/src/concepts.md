# Concepts

Most of `cy` should feel familiar to users of other terminal multiplexers such as `tmux`, but there are some new concepts that do not have parallels elsewhere.

## Groups and panes

A **pane** refers to a terminal window with a process running inside it, typically a shell or text editor. Every pane has a name. Panes in `cy` work exactly the same way that they do in `tmux`: you can have arbitrarily many panes open and switch between them on demand.

For the time being, `cy` only allows you to view and interact with one pane at once, which is centered inside of the **viewport** and automatically resized as you change the bounds of your terminal. By default, `cy` only allows a pane to expand to a width of 80 columns or the width of your terminal window, whichever is smaller, but this is configurable. This is practical because `cy` makes it easy to switch between panes.

Every pane `cy` belongs to a **group**. A group has a name and children, which consist of either panes or other groups.

The combination of groups and panes in cy form a tree that is similar to a filesystem. For example, a `cy` session may end up with a structure that looks like this:

```
/ (the root group, which has no name)
├── /my-project
│   ├── /pane-1
│   ├── /pane-2
│   └── /group-2
│       └── /pane-3
└── /another-group
```

For both panes and groups, you can define custom keybindings and set arbitrary parameters.

## Fuzzy finding

## Replay
