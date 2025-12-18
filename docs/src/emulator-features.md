# Terminal emulator features

`cy`'s internal terminal emulator is broadly compatible with `xterm` and fixes the value of `TERM` to `xterm-256color`.

`cy` supports the following extensions:

- [The kitty keyboard protocol](https://sw.kovidgoyal.net/kitty/keyboard-protocol/#progressive-enhancement): applications running in `cy` can request additional information about key events. Note that this requires using a terminal that supports this to connect to `cy`.
