# Keybindings

In `cy`, keybindings consist of a sequence of one or more keys that execute Janet code when you type them. You define new key sequences with the `(key/bind)` function.

For example:

```janet
(key/bind :root ["ctrl+l"] (fn [&] (cy/toast :info "you hit ctrl+l")))
```

This tells `cy` that whenever you type `ctrl+l` it should show a toast with the text "you hit ctrl+l".

The `(key/bind)` function takes three parameters:

1. **A scope**: The circumstances in which this binding should apply, such as a [group](./groups-and-panes.md) or mode (e.g. `:replay`). In this case we use the `:root` [keyword](https://janet-lang.org/docs/strings.html), which is a handy way of saying this binding should apply everywhere.
1. **A key sequence**: A Janet tuple that indicates the keys that must be typed for the callback to execute.
1. **A function**: The callback that should be executed when this key sequence matches.

## Key sequences

Key sequences in `cy` are more flexible than they appear at first glance. Valid sequences can consist of the following elements:
1. **Printable Unicode characters**: `你`, `Щ`, `a`
1. **Preset keys**: `return`, `ctrl+a`, `f1` You can find a comprehensive list of the available keys here.
1. Regexes: `[:re "^[a-z]$"]`
