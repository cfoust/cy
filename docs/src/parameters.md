# Parameters

`cy` has a **parameter** system for storing and retrieving Janet key-value pairs. Parameters are set with {{api param/set}} and retrieved with {{api param/get}}:

```janet
#          v- :root refers to the node tree's root node
(param/set :root :some-parameter true)

(param/get :some-parameter)
# true
```

Parameter values only stick around while the `cy` server is running.

Some parameters influence `cy`'s behavior. These are known as [**default parameters**](/default-parameters.md). You set them just like any other parameter:

```janet
(param/set :root :data-directory "~/some/other/dir")
```

Parameters work just like bindings do in the [node tree](/groups-and-panes.md#the-node-tree): parameter values in descendant nodes overwrite those in their ancestors. This allows `cy`'s functionality to be configured on a per-group (or even per-pane) basis.

## `:persist`

It can be useful to store a value across `cy` sessions. For this purpose, `cy` provides a special kind of parameter, `:persist`, that is written to a SQLite database stored on disk:

```janet
(param/set :persist :some-value @{:hello 2})
(param/get :some-value :target :persist)
```

The values you set using `:persist` are serialized using Janet's [marshal](https://janet-lang.org/api/index.html#marshal) function, so you can store any Janet value, even functions.

The location of the database used by `:persist` is stored in `$XDG_STATE_HOME/cy`.
