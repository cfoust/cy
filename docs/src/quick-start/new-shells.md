# Creating a new shell

{{story cast cy/shell}}

To create a new shell, type `ctrl+a` `j`. This creates a new pane running your default shell in your current working directory. You can return to the old one with `ctrl+l`; this cycles between all of the panes in the current **group**. In `cy`, a group is just a container for panes or other groups.

Every **group** and **pane** in `cy` has a path, just like a file in a filesystem. The new shell you created has a path like `/shells/3`.

The collection of all **groups** and **panes** is referred to as [**the node tree**](./groups-and-panes.md). When you first start `cy`, the node tree looks like this:

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
