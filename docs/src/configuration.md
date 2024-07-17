# Configuration

`cy` was designed to be configurable. All of its functionality is made available in an API that uses [Janet](https://janet-lang.org/), which is a fun, embeddable [Lisp-like](<https://en.wikipedia.org/wiki/Lisp_(programming_language)>) language that is easy to learn.

If you are new to Janet, I recommend starting out with its [documentation](https://janet-lang.org/docs/syntax.html) and Ian Henry's fantastic [_Janet for Mortals_](https://janet.guide/).

Janet looks like this:

```janet
(print "hello world!")
```

Users of `cy` provide a configuration written in Janet to define key bindings, set parameters, and set up `cy` in any way they like.

### Configuration files

On startup, `cy` will search for and execute the first file containing Janet source code that it finds in the following locations. (`cy` adheres to the [XDG base directory specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html).)

1. `$XDG_CONFIG_HOME/cy/cyrc.janet`
1. `$XDG_CONFIG_HOME/cyrc.janet`
1. `$XDG_CONFIG_HOME/.cy.janet`
1. `$HOME/cy/cyrc.janet`
1. `$HOME/cyrc.janet`
1. `$HOME/.cy.janet`
1. `$HOME/.config/cy/cyrc.janet`
1. `$HOME/.config/cyrc.janet`
1. `$HOME/.config/.cy.janet`

You can reload your configuration at runtime using {{api action/reload-config}}, which by default is bound to {{key :root ctrl+a r}}.

Your `cy` configuration can contain any valid Janet statement, but `cy` also provides additions to the standard library in the form of [an API](./api.md) for controlling every aspect of how `cy` works.

### Example configuration

An example configuration that uses functionality from this API is shown below. Very little of this will make sense right now; this is just to give you a taste of how configuration works in `cy` before moving on to the next section.

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
  # (cy/toast): shows a toast popup in the top-right corner of the screen
  (cy/toast :info (cmd/path (pane/current)))
  )

# Bind a key sequence to this function
(key/bind :root ["ctrl+a" "g"] toast-pane-path)
```
