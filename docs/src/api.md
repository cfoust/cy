# API

This is a complete listing of all of the API functions that `cy` provides in the top-level Janet environment (ie you do not need to `import` or `use` anything.)

Janet code executed with `cy` can also access everything from [Janet's standard library](https://janet-lang.org/api/index.html) except for anything in `spork`. In addition, Janet's `ev` family of functions probably will not work; I have never tested them.

## Concepts

#### Binding

Several API functions related to binding keys return a `Binding`. A `Binding` table represents a single key sequence and its associated function. Each table has the following properties:

- `:node`: the [NodeID](/api.md#nodeid) where the binding is defined.
- `:sequence`: a list of strings representing the key sequence that will execute this action. If the original call to {{api key/bind}} used a [regex](/keybindings.md#regexes), it will be returned as a string with a `re:` prefix.
- `:function`: the Janet function that will be called when this sequence is executed.

For example:

```janet
# ignore
{
  :node 1
  :sequence ["ctrl+a" "t"]
  :function <some function>
}
```

#### Color

Some API functions, such as {{api style/render}}, accept colors as input. `cy` supports True Color colors specified in hexadecimal along with ANSI 16 and ANSI-256 colors. Under the hood, `cy` uses [charmbracelet/lipgloss](https://github.com/charmbracelet/lipgloss?tab=readme-ov-file#colors) and thus supports its color references.

You can read more about color support in terminal emiulators [here](https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797#color-codes).

Examples of valid colors:

```janet
"#ffffff" # true color
"#123456" # true color
"255" # ANSI 256
"0" # ANSI 16
```

#### NodeID

Many API functions have a parameter of type `NodeID`, which can be one of two values:

- `:root` which is a short way of referring to `cy`'s top-level group.
- An integer that refers to a node in `cy`'s [node tree](/groups-and-panes.md#the-node-tree). You cannot infer these yourself, but they are returned from API functions like {{api pane/current}} and {{api group/children}}.

{{gendoc api}}
