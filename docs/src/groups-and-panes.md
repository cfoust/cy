# Groups and panes

`cy` calls open terminal sessions **panes** and stores them in a tree that behaves like a filesystem. This is referred to as **the node tree** which consists of **panes** and **groups**. The node tree is a simple abstraction for organizing panes and isolating settings and functionality to particular subsets of them.

## Panes

A **pane** refers to a terminal window with a process running inside it, typically a shell or text editor. Every pane has a name. Panes in `cy` work exactly the same way that they do in `tmux`: you can have arbitrarily many panes open and switch between them on demand.

## Groups

Every pane `cy` belongs to a **group**. A group has a name and children, which consist of either panes or other groups.

Groups also have two unique features:

- **Key bindings:** You may define key bindings that will only activate when you type that sequence while attached to any descendant of that group.
- **Parameters:** A key-value store that can be interacted with using {{api param/get}} and {{api param/set}}. Parameters are used both [to configure aspects of `cy`](/parameters.md) and also to create any functionality you desire by storing state in `cy`'s tree.

The preferred method of creating new groups is using the {{api group/mkdir}} function, which allows you to create many groups at once just like the real `mkdir` command.

For example:

```janet
(group/mkdir :root "/some/other/group")
```

The {{api group/mkdir}} function creates a group at the provided path (if it does not already exist) and returns its [NodeID](//api.md#nodeid).

## The node tree

A `cy` session may end up with a structure that looks like this:

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

### Inheritance

`cy`'s flexibility comes from the way key bindings and parameters interact:

- **Key bindings** are inherited down the tree, but can be overridden by descendant groups and panes.
- **Parameters** work the same way: {{api param/get}} will get the value of a parameter from the closest parent group or pane that defines it. Note that `:persist` parameters are stored globally and do not follow this inheritance pattern.

Imagine that you are attached to `/my-project/group-2/pane-3` in the example above:

- If `/my-project` defines a binding with the sequence <kbd>ctrl+a</kbd><kbd>b</kbd> and `/my-project/group-2` also defines one that begins with <kbd>ctrl+a</kbd>, the latter will take precedence.
- If `/my-project` defines a value for a parameter `:some-parameter` and `/my-project/group-2` does not, `(param/get :some-parameter)` will retrieve the value from `/my-project`.
- If the pane `/my-project/group-2/pane-3` defines a key binding, it can only be triggered when attached to that pane.

One of `cy`'s goals is for everything to be configured solely with key bindings and parameters; in this way `cy` can have completely different behavior depending on the environment and project.
