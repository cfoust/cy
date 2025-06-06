# Notifications

`cy` has two API calls for informing the user of something:

- {{api msg/log}}: Send a log message to the `/logs` pane, which the user can access like any other pane. Logs are also written to `/tmp/cy-$(id -u)/*.log`.
- {{api msg/toast}}: Show a toast message to the user. This message will appear in the top-right corner of their screen and disappear after a few seconds.

Both functions take a level (one of `:info`, `:warn`, or `:error`) and a message.

For example:

```janet
(msg/toast :info "this shows up in blue")
(msg/toast :warn "this shows up in yellow")
(msg/toast :error "this shows up in red")
```

You can configure the colors used for toasts with the [`color-*`](/default-parameters.md#color-error) family of parameters.

This code would show toasts that look like this:

{{story static toasts --width 60 --height 15}}

And for log messages:

```janet
# ignore
(msg/log :info "this shows up in blue")
(msg/log :warn "this shows up in yellow (does it though?)")
(msg/log :error "this shows up in red")
```

{{story static logs --width 80 --height 5}}
