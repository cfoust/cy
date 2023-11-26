# Groups and panes

`cy` contains a simple abstraction for isolating settings and functionality to a particular set of panes. This is referred to as **the node tree** which consists of **panes** and **groups**.

## Panes

A **pane** refers to a terminal window with a process running inside it, typically a shell or text editor. Every pane has a name. Panes in `cy` work exactly the same way that they do in `tmux`: you can have arbitrarily many panes open and switch between them on demand.

For the time being, `cy` only allows each user to view and interact with one pane at once, which is centered inside of the **viewport** and automatically resized as that user changes the bounds of their terminal.

By default, `cy` only allows a pane to expand to a width of 80 columns or the width of your terminal window, whichever is smaller, but this is configurable. This is practical because `cy` makes it easy to switch between panes.

## Groups

Every pane `cy` belongs to a **group**. A group has a name and children, which consist of either panes or other groups.

Groups also have two unique features:

- **Key bindings:** You may define key bindings that will only activate when you type that sequence while attached to any descendant of that group.
- **Parameters:** A key-value store that can be interacted with using `(cy/get)` and `(cy/set)`. Getting a parameter retrieves the value of that parameter that is nearest to the user's attached pane; this means that descendant groups can override the values present in their ancestor groups. Learn more on [the parameters page](./parameters.md).

## The node tree

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

Nodes in the tree can be referred to by their **path**, such as `/my-project/pane-1`. Node paths **are not required to be unique**, however. They are only presented as a convenient conceptual model for the user.

Instead, each node is permanently assigned a unique identifier (which is just an integer) referred to as a **node ID** and the related API calls only accept those IDs.
