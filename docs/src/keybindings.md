# Keybindings

In `cy`, keybindings consist of a sequence of one or more keys that execute Janet code when you type them. You define new key sequences with the `(key/bind)` function.

For example:

```janet
(key/bind :root ["ctrl+l"] (fn [&] (cy/toast :info "you hit ctrl+l")))
```

This tells `cy` that whenever you type `ctrl+l` it should show a toast with the text "you hit ctrl+l".

The `(key/bind)` function takes three parameters:

1. **A scope**: The circumstances in which this binding should apply, such as a [group](./groups-and-panes.md) or mode (e.g. `:replay`). In this case we use the `:root` [keyword](https://janet-lang.org/docs/strings.html), which is a handy way of saying this binding should apply everywhere.
1. **A key sequence**: A Janet [tuple](https://janet-lang.org/docs/data_structures/tuples.html) that indicates the keys that must be typed for the callback to execute.
1. **A function**: The callback that should be executed when this key sequence matches.

Scopes will be covered in a later chapter: here we will cover key sequences and functions at length.

## Key sequences

Key sequences in `cy` are more flexible than they appear at first glance. Valid sequences can consist of the following elements:

1. **Printable Unicode characters**: `你`, `Щ`, `a`
1. **Preset keys**: `return`, `ctrl+a`, `f1` You can find a comprehensive list of the available keys [here](./preset-keys.md).
1. **Regexes**: `[:re "^[a-z]$"]`

The first two work exactly as you expect them to: `cy` will execute the first complete match for the keys that you type. After each key, `cy` gives you a second (=1000ms) to type the next key in the sequence. If you do not, `cy` does nothing. All keys that are not matched by any sequence are sent to the current pane.

Here are some valid key sequences:

```janet
# This is a match when you type these three characters in succession
["a" "b" "c"]
# This works similarly to tmux's notion of "prefixes"
["ctrl+a" "a"]
["ctrl+a" "ж"] # unicode is OK
[" " "l"]
```

It is important to note that `cy` **does not send partial sequences to the current pane**. In other words, defining a sequence that begins with `" "` means that you will no longer be able to type the space character.

### Regexes

The most powerful aspect of `cy`'s keybinding engine is the ability to define key sequences that include [Perl-compatible](https://en.wikipedia.org/wiki/Perl_Compatible_Regular_Expressions) regular expressions. Each element that matches a regex **is passed to the callback as a string value.**

To illustrate:

```janet

(defn toast-me [key] (cy/toast :info key))
(key/bind :root ["ctrl+b" [:re "[abc]"]] toast-me)
```

Now if you type `ctrl+a` followed by `a`, the `toast-me` function will be invoked with one argument, the Janet value `"a"`. The same applies if you follow the `ctrl+a` with `b` or `c`.

This allows you to build more sophisticated functionality without defining a binding for every possible character.

A practical application of this can be found in `cy`'s source code, where we use this functionality to support `vim`-like character movements in [replay mode](./replay-mode.md):

```janet
(key/bind :replay ["f" [:re "."]] replay/jump-forward)
(key/bind :replay ["F" [:re "."]] replay/jump-backward)
```

[Key specifiers](./preset-keys.md) are matched as though their names were typed by the user; this means that providing the pattern `"ctrl\+[a-c]"` will match `ctrl+a`, `ctrl+b`, and `ctrl+c`.

Accessing individual match groups is not supported; functions always receive the full string that matched the pattern.

## Functions

Any Janet function can be passed as a callback to `(key/bind)`. The arity of that function should match the output of the provided sequence; for key sequences that do not include any regex patterns, this means that the function should not take any arguments.

Like `tmux`, many users at once can connect to the same `cy` server. The function provided to `(key/bind)` **is executed in the context of the user that invoked it**. Certain functions in `cy`'s API, such as `(pane/current)`, return information about the state of the current user, rather than the server as a whole. This means that if two users type the same sequence, they will get different results.

### Actions

In some cases it is inconvenient to have to provide functions directly to `(key/bind)`. For example, if you are writing a plugin, you might want to be able to provide new actions that a user can take without forcing them to use your key bindings. The user also may not want to assign all of your plugin's functionality to arcane bindings they won't remember.

To assist with this, `cy` has a system for **actions**, which are similar in nature to commands [in VSCode](https://code.visualstudio.com/api/extension-guides/command) or [in Sublime Text](https://docs.sublimetext.io/reference/commands.html). An action consists of a short description and a function. When the user opens the command palette (which is bound by default to `"ctrl+a" "ctrl+p"`), they can search for and execute an action based on that description.

You define new actions using the `(key/def)` macro. Here is an example from `cy`'s source code:

```janet
(key/def
  # The identifier to which this action will be bound
  # This is never shown in the UI
  cy/kill-current-pane
  # The docstring
  # The user uses this to find the action you define
  "kill the current pane"
  # All subsequent forms comprise the body of the action, or the lines of code
  # that will be executed when it is invoked
  (tree/kill (pane/current)))

(key/bind :root ["ctrl+b" "b"] cy/kill-current-pane)
```

`(key/def)` actually just invokes Janet's `(defn)` macro under the hood. This means that actions are just ordinary Janet functions that happen to be registered with `cy`. `(key/def)` exists so that you can clearly identify to the user the functionality your plugin provides.

You can also just use actions to avoid memorizing a key binding you rarely use:

```janet
(key/def
  thing-i-rarely-do
  "this is something I do once a year"
  (pp "hi"))
```
