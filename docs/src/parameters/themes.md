# Themes

You can configure almost anything about `cy`'s appearance by changing [default parameters](/default-parameters.md) to your liking. This includes colors, border styles, and UI copy (such as for localization.)

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
