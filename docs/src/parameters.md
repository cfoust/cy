# Parameters

`cy` contains a primitive key-value store it refers to as parameters. In addition to being available for use from Janet for arbitrary purposes, parameters are also the primary means of configuring `cy`'s behavior.

Parameters are set with {{api param/set}} and retrieved with {{api param/get}}:

```janet
(param/set :some-parameter true)

(param/get :some-parameter)
# returns true
```

## Default parameters

Some parameters are used by `cy` to change how it performs certain operations.

| Parameter        | Default                                                                   | Description                                                                             |
| ---------------- | ------------------------------------------------------------------------- | --------------------------------------------------------------------------------------- |
| `:data-dir`      | [inferred on startup](replay-mode.md#recording-terminal-sessions-to-disk) | the directory in which `.borg` files are saved; if empty, recording to file is disabled |
| `:animate`       | `true`                                                                    | whether animations are enabled (disabled over SSH connections by default)               |
| `:default-shell` | inferred from `$SHELL` on startup                                         | the default command used for {{api cmd/new}}                                            |
