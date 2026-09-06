# x11

[![PkgGoDev](https://pkg.go.dev/badge/golang.design/x/x11)](https://pkg.go.dev/golang.design/x/x11)

A minimal, **pure-Go** client for the X11 wire protocol — no Cgo, no `libX11`.

```
import "golang.design/x/x11"
```

Package `x11` implements the encoding, decoding, and parsing for the slice of
the [X Window System Protocol, version 11](https://www.x.org/releases/current/doc/xproto/x11protocol.html)
needed to talk to an X server directly over its socket:

- `$DISPLAY` parsing (unix / abstract / TCP)
- `.Xauthority` parsing and `MIT-MAGIC-COOKIE-1` selection
- connection setup request + setup-reply parsing, resource-ID allocation
- requests: `InternAtom`, `CreateWindow`, `Set`/`GetSelectionOwner`,
  `ConvertSelection`, `ChangeProperty`, `GetProperty`, `DeleteProperty`,
  `SendEvent`, `GetInputFocus`
- packet reading and reply/event/error decoding

It contains **only** the wire logic — no sockets. Callers own the transport
(`net.Conn`, an `io.Reader`, …), which keeps the package free of OS-specific
code and unit-testable on any platform.

This package was extracted from [`golang.design/x/clipboard`](https://github.com/golang-design/clipboard),
where it backs the Cgo-free X11 clipboard on Linux and the BSDs.

## Example

Sketch of reading the connection setup over a dialed socket:

```go
conn, _ := net.Dial("unix", "/tmp/.X11-unix/X0")
conn.Write(x11.SetupRequest("", nil)) // or a MIT-MAGIC-COOKIE-1 from .Xauthority
setup, err := x11.ReadSetup(bufio.NewReader(conn))
if err != nil {
	// ...
}
ids := x11.NewIDGen(setup)
win := ids.Next()
conn.Write(x11.CreateWindow(win, setup.Root))
```

See [`golang.design/x/clipboard`](https://github.com/golang-design/clipboard)'s
X11 backend for a complete, working consumer.

## License

MIT &copy; The golang.design Initiative Authors
