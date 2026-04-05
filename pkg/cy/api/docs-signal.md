# doc: Send

(signal/send name)

Signal all goroutines waiting on the channel `name`, which is a keyword. Any call to {{api signal/wait}} with the same name will unblock. If no one is waiting, the signal is silently dropped.

This is useful for inter-process coordination, for example between a `cy exec` script and a running pane.

```janet
# Signal from one process
(signal/send :my-channel)
```

# doc: Wait

(signal/wait name &named timeout)

Block until the channel `name` is signaled via {{api signal/send}}. `name` is a keyword. Multiple goroutines can wait on the same channel; all will be woken when the channel is signaled.

Optional named parameters:
- `:timeout` (number, default 0): Maximum number of seconds to wait. A value of 0 means wait indefinitely. Returns an error if the timeout is reached.

```janet
# Wait with a 10-second timeout
(signal/wait :my-channel :timeout 10)
```
