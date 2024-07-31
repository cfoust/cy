# Configuration

`cy` is not really a terminal multiplexer. It's actually an API for making terminal multiplexers that happens to come with a decent default configuration. This is because `cy` was designed to be as configurable as possible:

1. Features are first built to expand the functionality available in the API.
2. They then are incorporated into the default configuration where appropriate.

As a result, `cy` is the best choice for users who enjoy tinkering with the tools they use.

`cy` uses [Janet](https://janet-lang.org/) as its configuration language. Janet is a fun, embeddable [Lisp-like](https://en.wikipedia.org/wiki/Lisp_(programming_language)) language that is easy to learn. If you are new to Janet, I recommend starting out with its [documentation](https://janet-lang.org/docs/syntax.html) and Ian Henry's fantastic [_Janet for Mortals_](https://janet.guide/).

> #### Why Janet?
> This might come as a surprise, but I am far from being a Lisp zealot and have
> never seriously used `emacs`. I found Janet several years ago and enjoyed
> experimenting with it. The language avoids most of the scary levels of
> abstraction present in some Lisps and strikes a balance between functional
> programming orthodoxy and practical imperative programming needs.

### Configuration files

On startup, `cy` will search for and execute the first file containing Janet source code that it finds in the following locations. `cy` adheres to the [XDG base directory specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html).

1. `$XDG_CONFIG_HOME/cy/cyrc.janet`
1. `$XDG_CONFIG_HOME/cyrc.janet`
1. `$XDG_CONFIG_HOME/.cy.janet`
1. `$HOME/cy/cyrc.janet`
1. `$HOME/cyrc.janet`
1. `$HOME/.cy.janet`
1. `$HOME/.config/cy/cyrc.janet`
1. `$HOME/.config/cyrc.janet`
1. `$HOME/.config/.cy.janet`

You can reload your configuration at any time using {{api action/reload-config}}, which by default is bound to {{bind :root ctrl+a r}}.

### Example configuration

An example configuration that uses functionality from this API is shown below. You may also refer to the [default configuration](https://github.com/cfoust/cy/blob/main/pkg/cy/boot) for examples of more advanced API usage.

```janet
# Define a new action (which is just a function) with the name toast-pane-path
(key/action
  # the name of the function by which it can be referenced in the current
  # scope
  toast-pane-path
  # a (required) docstring for the action
  "show the path of the current pane"
  # all subsequent statements are executed when this action is invoked (usually
  # by a key binding)
  #
  # (pane/current): gets the ID of the current pane
  # (cmd/path): gets the path of the pane with the given ID
  # (msg/toast): shows a toast popup in the top-right corner of the screen
  (msg/toast :info (cmd/path (pane/current))))

# Bind a key sequence to this function
(key/bind :root ["ctrl+a" "g"] toast-pane-path)
```

### Execution context

The Janet code executed in `cy` can be executed in two different contexts:

- **The context of the server:** This is the context in which your configuration file is run, because when the `cy` server starts up, there are no clients.
- **The context of a client:** When a client invokes an [action](/keybindings.md#actions) or types a keybinding sequence, the executed Janet code can see what client ran the action and respond appropriately.

Because of this, _some API functionality can only be used in a keybinding or action._ This is because some kinds of state, such as the state that `(viewport/*)` family of functions uses to work, only makes sense in the context of a user.

This is a confusing aspect of `cy` that I plan to improve over time, such as by letting you execute custom code in the context of a client whenever one connects.
