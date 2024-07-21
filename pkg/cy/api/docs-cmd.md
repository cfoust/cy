# doc: New

(cmd/new parent &named path command args name)

Run `command` with `args` and working directory `path` in a new pane as a child of the group specified by `parent`. You may also provide the `name` of the new pane. If `command` is not specified, `(cmd/new)` defaults to the current user's shell. `parent` is a [NodeID](/api.md#nodeid).

When the command exits, it will be rerun. This is not currently configurable. If the command exits with a non-zero exit code more than three times in a second, it will not be run again.

Some examples:

```janet
# Create a new shell in the root group
(cmd/new :root)

# `args` is a list of strings
(cmd/new :root :command "less" :args @["README.md"])
```

# doc: Path

(cmd/path target)

Get the working directory of the program running in the pane pane specified by `target`. `target` is a [NodeID](/api.md#nodeid).
