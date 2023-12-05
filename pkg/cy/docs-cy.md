# doc: KillServer

Kill the `cy` server, disconnecting all clients.

# doc: Detach

Detach from the `cy` server.

# doc: Get

(cy/get key)

Get the value of the [parameter](./parameters.md) with key `key`.

# doc: Set

(cy/set key value)

Set the value of the [parameter](./parameters.md) with key `key` to value `value`.

# doc: Replay

Enter replay mode for the current pane.

# doc: Paste

Paste the text in the copy buffer to the current pane.

# doc: Toast

(cy/toast level message)

Send a toast with `message` to all attached clients. `level` must be one of `:info`, `:warn`, `:error`.

# doc: Log

(cy/log level message)

Log `message` to the `/logs` pane. `level` must be one of `:info`, `:warn`, `:error`.
