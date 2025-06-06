# Creating a shell

{{story cast cy/shell}}

To create a new pane, type {{bind :root ctrl+a j}}. This starts a pane with your default shell in your current working directory. You can return to the old one with {{bind :root ctrl+l}}; this cycles between all of the panes in the current **group**. In `cy`, a group is just a container for panes or other groups.

You remove panes using {{bind :root ctrl+a x}}.

Every **group** and **pane** in `cy` has a path, just like a file in a filesystem. The new shell you created has a path like `/shells/3`.

The collection of all **groups** and **panes** is referred to as [**the node tree**](/groups-and-panes.md). When you first start `cy`, the node tree looks like this:

```
/ (group)
├── /shells (group)
│   └── /2 (pane) <- you start here
└── /logs (pane)
```

And here's how it looks after you create a new shell with {{bind :root ctrl+a j}}:

```
/ (group)
├── /shells (group)
│   ├── /2 (pane)
│   └── /3 (pane) <- now you're here
└── /logs (pane)
```

When you are attached to a pane in the `/shells` group, {{bind :root ctrl+l}} cycles to the next sibling pane.
