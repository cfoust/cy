# CLI

`cy` supports a range of command line options. For the most authoritative information relevant to your version of `cy`, run `cy --help`.

All `cy` functionality is divided into subcommands, which are executed using `cy <subcommand>`. If you do not provide a subcommand, `cy` defaults to the `connect` subcommand described below.

### The `--socket-name` flag

Just like `tmux`, `cy` supports running multiple servers at once. All subcommands support the `--socket-name` (short: `-L`) flag, which determines which `cy` server to connect to. For example, to start a new `cy` server named `foo`, you would run `cy --socket-name foo`.

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
