# doc: KillServer

Kill the `cy` server, disconnecting all clients.

# doc: Detach

Detach from the `cy` server.

# doc: Paste

Paste the text in the copy buffer to the current pane.

# doc: ReloadConfig

Detect and (re)evaluate cy's configuration. This uses the same configuration detection scheme described in [the Configuration chapter](/configuration.md#configuration-files).

# doc: Trace

Save a 15-second trace captured with [runtime/trace](https://pkg.go.dev/runtime/trace) to the socket directory. This is only useful for debugging.

# doc: CpuProfile

Save a 15-second CPU profile captured with [pprof](https://go.dev/blog/pprof) to the socket directory. This is only useful for debugging.

# doc: Id

Get the unique integer identifier for the current client.
