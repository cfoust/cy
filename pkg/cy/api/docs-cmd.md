# doc: New

(cmd/new parent &named path restart command args name)

Run `command` with `args` and working directory `path` in a new pane as a child of the group specified by `parent`. You may also provide the `name` of the new pane. If `command` is not specified, `(cmd/new)` defaults to the current user's shell. `parent` is a [NodeID](/api.md#nodeid).

If `restart` is `true`, when the command exits, it will be rerun. However, if the command exits with a non-zero exit code more than three times in a second, it will not be run again.

Some examples:

```janet
# Create a new shell in the root group
(cmd/new :root)

# `args` is a list of strings
(cmd/new :root :command "less" :args @["README.md"])
```

# doc: Path

(cmd/path target)

Get the working directory of the program running in the pane specified by `target`. `target` is a [NodeID](/api.md#nodeid).

# doc: Title

(cmd/title target)

Get the terminal title of the pane specified by `target`. Returns the title as set by the program running in the terminal (e.g. via OSC escape sequences). `target` is a [NodeID](/api.md#nodeid).

# doc: Pid

(cmd/pid target)

Get the process ID (PID) of the command running in the pane specified by `target`. `target` is a [NodeID](/api.md#nodeid).

# doc: Commands

(cmd/commands target)

Get the commands executed in a particular pane. Returns an array of [Commands](/api.md#command). `target` is a [NodeID](/api.md#nodeid).

# doc: Query

Query all of the commands stored in the [command database](/command-history.md).

# doc: Kill

(cmd/kill target)

Kill the pane specified by target. `target` is a [NodeID](/api.md#nodeid).

# doc: Execute

(cmd/execute args &named env stdin)

Execute a command and return its output. This is useful for running shell commands and capturing their output directly in Janet.

Note: Since cy does not use Janet's event loop, the built-in [`(os/execute)`](https://janet-lang.org/api/index.html#os/execute) does not work.

`args` is an array/tuple of strings where the first element is the command and the rest are arguments (same as `(os/execute)`).

Optional named parameters:
- `:env` (struct): Additional environment variables to set.
- `:stdin` (string): Input to pass to the command's standard input.

Returns a struct with:
- `:stdout` (string): The command's standard output.
- `:stderr` (string): The command's standard error.
- `:exit-code` (int): The exit code (0 indicates success).

Subprocesses can communicate with the cy server via `cy exec`.

A non-zero exit code is not treated as an error; check the `:exit-code` field to determine if the command succeeded.

Some examples:

```janet
# Run a simple command
(def result (cmd/execute ["echo" "hello"]))
(print (result :stdout))  # => "hello\n"

# Pass input via stdin
(def result (cmd/execute ["cat"] :stdin "hello world"))
(print (result :stdout))  # => "hello world"

# Set environment variables
(def result (cmd/execute ["sh" "-c" "echo $MY_VAR"] :env {"MY_VAR" "hello"}))
(print (result :stdout))  # => "hello\n"

# Check exit code
(def result (cmd/execute ["false"]))
(print (result :exit-code))  # => 1
```
