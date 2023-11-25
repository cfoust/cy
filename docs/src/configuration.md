# Configuration

All of `cy`'s behavior is determined using a programming language called [Janet](https://janet-lang.org/), which is a fun, embeddable [Lisp](<https://en.wikipedia.org/wiki/Lisp_(programming_language)>) that is easy to learn and comes out of the box with a comprehensive standard library. If you are new to Janet, I recommend starting out with its [documentation](https://janet-lang.org/docs/syntax.html) and Ian Henry's fantastic [_Janet for Mortals_](https://janet.guide/).

Janet looks like this:

```
(print "hello world!")
```

If that doesn't scare you, read on.

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

Your `cy` configuration can contain any valid Janet statement, but `cy` also provides additions to the standard library in the form of [an API](./api.md) for controlling every aspect of how `cy` works.

### Example configuration

An example configuration that uses functionality from this API is shown below. Very little of this will make sense right now; this is just to give you a taste of how configuration works in `cy` before moving on to the next section.

```janet
# Define a new action (which is just a function) with the name toast-pane-path
(key/def
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
(key/bind :root ["ctrl+a" "g"] caleb/test)
```
