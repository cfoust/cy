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

You can read more about color support in terminal emulators [here](https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797#color-codes).

Examples of valid colors:

```janet
"#ffffff" # true color
"#123456" # true color
"255" # ANSI 256
"0" # ANSI 16
```

#### Color map

A color map is a mapping from one [color](/api.md#color) to another. They can represented by Janet structs or tables:

```janet
# The "zenburn" color map
# This maps the default 16 ANSI colors to hand-picked RGB colors
@{"0" "#383838"
  "1" "#dca3a3"
  "2" "#5f7f5f"
  "3" "#e0cf9f"
  "4" "#7cb8bb"
  "5" "#dc8cc3"
  "6" "#93e0e3"
  "7" "#dcdccc"
  "8" "#6f6f6f"
  "9" "#dfaf8f"
  "10" "#404040"
  "11" "#606060"
  "12" "#808080"
  "13" "#c0c0c0"
  "14" "#000000"
  "15" "#ffffff"}
```

For more information about color maps, see the [relevant section](/parameters/colors.md#color-maps) in the parameters chapter.

#### Command

{{api cmd/query}} and {{api/commands}} return Commands, which are structs that look like this:

```janet
# ignore
{:text "echo 'Hello, World!'" # The command that was run
 :directory "/some/dir"
 # When the command started executing
 :executed-at {} # Time
 # When it finished
 :completed-at {} # Time
 # These three properties only have internal meaning for now
 :prompted 23
 :executed 26
 :completed 52
 :pending false
 # Input and output are Selections, which can currently only be passed to
 # input/find previews. Refer to the standard library.
 :input @[{:from (97 6)
           :to (97 7)}]
 :output {:from (98 0)
          :to (98 42)}}
```

#### NodeID

Many API functions have a parameter of type `NodeID`, which can be one of two values:

- `:root` which is a short way of referring to `cy`'s top-level group.
- An integer that refers to a node in `cy`'s [node tree](/groups-and-panes.md#the-node-tree). You cannot infer these yourself, but they are returned from API functions like {{api pane/current}} and {{api group/children}}.

#### Time

The `(time/*)` family of functions works with `Time` values, which are similar (but not identical) to the value returned by Janet's built-in `(os/date)` function.

Time values are structs with the following properties:

```janet
{:dst false # unused, just to mimic (os/date)
 :hours 10
 :milliseconds 624
 :minutes 15
 :month 9
 :month-day 15
 :seconds 31
 :utc false # if false, properties are in local time
 :week-day 0
 :year 2024
 :year-day 259}
```

For specifics on the range of each scalar property, consult the documentation for Go's [time package](https://pkg.go.dev/time), such as for [time.Weekday()](https://pkg.go.dev/time#Time.Weekday).

#### Style

A style struct is used with {{api style/render}} to apply styling effects to text. Style structs can contain any of the following properties:

- `:fg`: The foreground [color](/api.md#color) of the text.
- `:bg`: The background [color](/api.md#color) of the text.
- `:width`: The number of horizontal cells the text should occupy. Padding is added if this value exceeds the length of the text.
- `:height`: The number of vertical cells the text should occupy. Padding is added if this value exceeds the height of the text.
- `:align-horizontal`: One of `:left`, `:center`, or `:right`. If `:width` is greater than the length of the text, the text will be aligned according to this property.
- `:align-vertical`: One of `:top`, `:center`, or `:bottom`. If `:height` is greater than the height of the text, the text will be aligned according to this property.
- `:bold`: A boolean indicating whether the text should be bolded.
- `:italic`: A boolean indicating whether the text should be italic.
- `:underline`: A boolean indicating whether the text should be underlined.
- `:strikethrough`: A boolean indicating whether the text should be struck through.
- `:reverse`: A boolean indicating whether the foreground and background color should be reversed.
- `:blink`: A boolean indicating whether the text should blink.
- `:faint`: A boolean indicating whether the text should be faint.

For example:

```janet
{:bg "4"
 :bold true
 :width 15}
```

{{gendoc api}}
