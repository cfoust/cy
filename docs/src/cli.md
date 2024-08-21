# CLI

`cy` supports a range of command line options. For the most authoritative information relevant to your version of `cy`, run `cy --help`.

All `cy` functionality is divided into subcommands, which are executed using `cy <subcommand>`. If you do not provide a subcommand, `cy` defaults to the `connect` subcommand described below.

### The `--socket-name` flag

Just like `tmux`, `cy` supports running multiple servers at once. All subcommands support the `--socket-name` (short: `-L`) flag, which determines which `cy` server to connect to. For example, to start a new `cy` server named `foo`, run `cy --socket-name foo`.

## Subcommands

### connect

`cy connect` connects to a `cy` server, starting a new one if there isn't one already running. It is similar to `tmux attach`.

### exec

`cy exec` runs Janet code on the `cy` server. This is useful for controlling `cy` programmatically, such as from a shell script or other program.

If you run `cy exec` in a terminal session inside of `cy`, it will infer the client on behalf of whom the Janet code should be run. This means that API functions that take over the client's `cy` session, like {{api input/find}}, will work as expected.

Some examples:

```bash
# Create a new cy shell in the current directory and attach the client to it
cy exec -c "(shell/attach \"$(pwd)\")"

# Set a parameter on the client
cy exec -c "(param/set :client :default-frame 'big-hex')"
```

#### Reading data from `cy`

Janet code run using `cy exec` can also return values using `(yield)`, which will be printed to standard output in your desired format. In addition to letting you use `cy`'s `input/*` API functions for getting user input in arbitrary shell scripts, you can also read any state you want, such as parameters, from the `cy` server.

```bash
cy exec -c "(yield (param/get :default-frame))"
# Output: big-hex
```

`cy exec` supports the `--format` flag, which determines the output format of `(yield)`ed Janet values. Valid values for `--format` are `raw` (default), `json`, and `janet`.

##### `raw`

`raw` is designed for easy interoperation with other programs. Primitive types such as strings, numbers, and booleans are printed as-is, without any additional formatting. For example, a string value `"hello"` will be printed as `hello`. Non-primitive types such as structs and tables cannot be printed in `raw` format.

##### `json`

`json` is designed for easy interoperation with other programs that can parse JSON, such as `jq`. All Janet values are converted to JSON, and the resulting JSON string is printed. For example, a table value `{:a 1}` will be printed as `{"a":1}`. Any Janet value that cannot be represented in JSON, such as functions, will cause an error.

For example, the following code gets the current [layout](/layouts.md) and prints it as JSON:

```bash
$ cy exec -c "(yield (layout/get))" -f json | jq
{
    "rows": 0,
    "cols": 80,
    "type": "margins",
    "node": {
        "id": 4,
        "attached": true,
        "type": "pane"
    }
}
```

##### `janet`

The `janet` format prints the `(yield)`ed value as a valid Janet expression. This is useful for debugging and for passing Janet values between `cy` and other Janet programs. However, just like `json`, the `janet` formatter does not support printing complex values like functions.

### recall

> For this to work, you must have [enabled command detection](/command-detection.md#enabling-command-detection) and `cy` must be installed on your system (ie available in your `$PATH`.)

`cy recall <reference>` prints the output of a command run in `cy` to standard output. In other words, if you run a command and later need to filter its output or pipe it to a file, you can do so without rerunning the command. `<reference>` is an identifier for a command run in `cy`.

#### Relative references

Inside of a pane in `cy`, running `cy recall -1` will write the output of the most recent command to standard output, e.g.:

```bash
cy recall -1 | grep 'some string'
cy recall -1 > out.log
```

A negative number refers to a command relative to the end of the "list" of all commands run in the current pane so far. So `cy recall -2` refers to the second-latest command.

`cy recall -1` can also be written as `cy -1`, a la:

```bash
cy -1 | grep 'some string'
cy -1 > out.log
```

Note that running `cy -1` in two different panes will produce different output; `cy` understands where you run a `cy` command and uses that context to direct your query.

#### Absolute references

`recall` also supports absolute references in the form `[[server:]node:]index`.

You do not have to come up with these yourself. Running the {{api action/recall-command}} action will let you choose a command, after which a `cy <reference>` command will be written to your shell.

`index` can be any integer and it refers to the index of a command inside of a pane starting from `0`. The command referred to by `cy -1` changes on every command run; `cy 0` on the other hand will always refer to the first command run in the pane.

`server` and `node` are optional. These properties are used by references generated by `cy` to disambiguate a reference to a particular command.

* `node` is an integer [NodeID](/api.md#nodeid) that specifies the pane from which the command will be read.
* `server` is the name of the socket the `cy` server is running on (the value of the `--socket-name` flag above).

Both `server` and `node` can be derived by `cy` when `cy recall` is run in a pane in a `cy` server, but if `server` is specified, you can also run `cy recall` _outside of a cy server:_ `cy recall default:0:1`.
