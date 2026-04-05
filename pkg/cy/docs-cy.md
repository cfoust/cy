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

# doc: Version

Get version information about the cy server. Returns a struct with the following fields:

- `:version`: The cy version string.
- `:go-version`: The Go version used to build the server.
- `:git-commit`: The Git commit hash of the build.
- `:build-time`: The time the server was built.

# doc: Signal

(cy/signal name)

Signal all goroutines waiting on the named channel `name`. Any call to {{api cy/wait-for}} with the same name will unblock. If no one is waiting, the signal is silently dropped.

This is useful for inter-process coordination, for example between a `cy exec` script and a running pane.

```janet
# Signal from one process
(cy/signal "my-channel")
```

# doc: WaitFor

(cy/wait-for name &named timeout)

Block until the named channel `name` is signaled via {{api cy/signal}}. Multiple goroutines can wait on the same channel; all will be woken when the channel is signaled.

Optional named parameters:
- `:timeout` (number, default 0): Maximum number of seconds to wait. A value of 0 means wait indefinitely. Returns an error if the timeout is reached.

```janet
# Wait with a 10-second timeout
(cy/wait-for "my-channel" :timeout 10)
```

# doc: Id

Get the unique integer identifier for the current client.
