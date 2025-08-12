# Theming

You can configure almost anything about `cy`'s appearance by changing [default parameters](/default-parameters.md) to your liking. This includes terminal themes, UI colors, border styles, and UI copy (such as for localization.)

Parameters that affect visual styling work in the same way all parameters do: setting a parameter for a group affects the visual styling of all descendant panes, unless a pane or child group defines a different value for the parameter.

### Color maps

Color maps are similar to color schemes. A color map is a mapping from one color to another. When `cy` renders a pane, it can use a color map that you define to translate the colors on the screen to change their appearance. This translation *does not affect the underlying recording for a pane*.

Most terminal programs only use 16 ANSI colors. All of `cy`'s built-in interfaces also obey this rule. Terminal emulators support color schemes by allowing the user to set the actual RGB colors used to represent those 16 colors on the screen.

In `cy`, this works by using a color map to translate those ANSI colors to RGB true colors. For example:

```janet
# The built-in "zenburn" color map
(param/set :root :color-map @{
  "0" "#383838" # Map ANSI 0 -> RGB color #383838
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
  "15" "#ffffff"})
```

Like any other parameter, {{param color-map}} can be set on a per-pane or per-group basis.

The important distinction between a color map and a color scheme is that color maps in `cy` allow you to map _any_ color to _any other color_, not just the 16 ANSI colors as is the case with traditional terminal emulators. For example, if a program you use insists on hard-coding ANSI256 or RGB colors, you can swap them to something else:

```janet
(param/set :root :color-map @{
  # Map RGB "#ff0000" -> ANSI16 1 (red)
  "#ff0000" "1"
  # Map ANSI 256 -> ANSI 1
  "123" "1"})
```

`cy` comes with several hundred built-in color maps from the [tinted-theming project](https://github.com/tinted-theming/home). You can see a gallery of them [here](https://tinted-theming.github.io/base16-gallery/). Every built-in color map is identified by a unique keyword ID such as `:google-dark`.

#### API

The API has several functions for working with the built-in color maps:

- {{api color-maps/get-all}}: Get all of the color maps.
- {{api color-maps/get}}: Get a color map by its ID.
- {{api color-maps/set}}: Set the color map for a pane or group using a color map ID. This also sets the `:color-map-id` parameter for the node, which is a convention (but not a requirement) used for built-in color maps.
- {{api color-maps/get-id}}: Get the value of the `:color-map-id` parameter for the given node.

The inheritance behavior of parameters can be used to apply a color map to entire groups of panes and also override the color map defined by an ancestor group. For example, consider the following Janet code:

```janet
# All panes by default will use the built in atelier-forest-light color map
(color-maps/set :root :atelier-forest-light)
# All shells (created by (shell/new)) will use a different scheme. Since
# /shells is a child of :root (a.k.a /), its :color-map will take precedence.
(color-maps/set (group/mkdir :root "/shells") :atelier-estuary)
```

#### Actions

Using the {{api action/set-pane-colors}} action you can quickly set the color map for a particular pane:

{{story cast color-maps}}

### Built-in UI

`cy` defines a wide range of parameters that are used for affecting the visual styling of its built-in UIs, such as:

- Replay mode
- Search mode
- The `(input/*)` family of API functions, for fuzzy finding et al
- Toast colors

For example:

{{story static theme}}

This theme was created with the following Janet code:

```janet
(param/set-many :root
                :replay-text-copy-mode "копировка"
                :replay-status-bar-bg "#6699cc"
                :replay-copy-bg "#99cc99"
                :replay-copy-fg "#2d2d2d"
                :input-prompt-bg "#f2777a"
                :color-error "#f2777a"
                :color-warning "#ffcc66"
                :color-info "#6699cc"
                :timestamp-format "начало революции: 2006-01-02 15:04:05")
```

Theme parameters work exactly like any other parameter. If you set a parameter with a target of `:client`, it overrides any other parameter. Parameters set on [tree nodes](/groups-and-panes.md#groups) override the parameter values of their ancestor nodes.
