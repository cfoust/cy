# Parameters

`cy` contains a key-value referred to as **parameters**. In addition to being available for use from Janet for arbitrary purposes, parameters are also the primary means of configuring `cy`'s behavior.

Parameters are set with {{api param/set}} and retrieved with {{api param/get}}:

```janet
(param/set :root :some-parameter true)

(param/get :some-parameter)
# returns true
```

Parameters work just like bindings do in the [node tree](./groups-and-panes.md#the-node-tree) in that the parameter values in descendant nodes overrite those in their ancestors when a client is attached to them. This allows `cy`'s functionality to be configured on a per-group (or even per-pane) basis using **default parameters**, which are described below.

## Default parameters

Some parameters are used by `cy` to change how it behaves.

| Parameter        | Default                                                                   | Description                                                                             |
| ---------------- | ------------------------------------------------------------------------- | --------------------------------------------------------------------------------------- |
| `:data-dir`      | [inferred on startup](replay-mode.md#recording-terminal-sessions-to-disk) | the directory in which `.borg` files are saved; if empty, recording to file is disabled |
| `:animate`       | `true`                                                                    | whether animations are enabled (disabled over SSH connections by default)               |
| `:default-shell` | inferred from `$SHELL` on startup                                         | the default command used for {{api cmd/new}}                                            |
