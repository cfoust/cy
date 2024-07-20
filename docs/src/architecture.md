# Architecture

This document is intended to be a brief introduction to `cy`'s code structure and its commonly used abstractions. The intended audience is anyone interested in contributing to `cy` or any of its constituent libraries, some of which may (eventually) be broken out into separate projects.

It is safe to assume that the high-level description in this document will remain reliable despite changes in the actual implementation, but if you are ever in doubt:

1. Read the README for the package you are modifying. Most [packages](./packages.md) in `pkg` have their own READMEs (along with some sub-packages.)
2. Ask for help [in Discord](https://discord.gg/NRQG3wbWGM).
3. Consult the code itself.

`cy` is written in Go and Janet. I chose Go because I had written other projects with significant concurrency needs and it seemed like a natural fit. Janet is a Lisp-like scripting language that I chose because it sounded like fun.

This document assumes basic familiarity with Go.

## Introduction

`cy` is a [**terminal multiplexer**](https://en.wikipedia.org/wiki/Terminal_multiplexer). Just like `tmux`, it uses a server-client model and daemonizes itself on server startup. In simple terms this means that irrespective of where, when, or how you start `cy`, if a `cy` server is running you can connect to it and resume your work exactly as you left it. Clients connect to the `cy` server using a WebSocket connection via a [Unix domain socket](https://en.wikipedia.org/wiki/Unix_domain_socket).

As the name "terminal multiplexer" implies, most of the complexity comes from doing two things:

1. **Emulating a terminal**: Just like in `tmux` et al, `cy` works by pretending to be a valid VT100 terminal and attaching to the programs that you run (typically shells).
2. **Multiplexing**: Users expect to be able to switch between the terminals `cy` emulates in order to fulfill the basic requirement of being a terminal multiplexer.

Terminal emulation, though tedious and error-prone to write yourself, is critical for any terminal multiplexer. Because of the paucity of Go libraries that accomplish this, this was implemented mostly from scratch in [the emu package](https://github.com/cfoust/cy/tree/main/pkg/emu).

Multiplexing, of course, is where things get interesting. `cy`'s codebase has a range of different tools for compositing and rendering terminal windows, all of which it does to be able to support an arbitrary number of clients, all of whom may have different screen sizes and need to use `cy` for different things.

`cy`'s main feature is being able to replay terminal sessions. You would think that it would be a source of significant complexity. But it really isn't: once you have the above, making this functionality is just a matter of recording every write to a virtual terminal, then replaying it on demand. Of course, the devil is in the details.

## Codebase organization

`cy`'s code is divided into three directories found at the repository root:

- `cmd`: Contains the code for all executables (in this case, programs with `main.go` files.)
  - `cy`: The main `cy` executable and the code necessary to connect to and create sockets.
  - `stories`: A system for quickly iterating on `cy`'s visual design. Covered in more detail in [a dedicated chapter](./stories.md).
  - `perf`: A (seldom-used) program for testing the performance of `cy`'s history search feature.
  - `docs`: A simple executable that dumps various information about `cy` to standard out as JSON, such as all of its API functions, built in key bindings, et cetera. This is used in an [mdbook preprocessor](https://rust-lang.github.io/mdBook/format/configuration/preprocessors.html) called [gendoc](https://github.com/cfoust/cy/blob/main/docs/gendoc.py) that generates Markdown content for `cy` on demand.
- `pkg`: Contains a range of different Go packages, all of which might be charitably called libraries. The list below is not intended to be exhaustive, but just highlight several important ones.
  - `cy`: The `cy` server, API, default configuration, et cetera.
  - `geom`: Simple, high-level geometric primitives (think `Vec2`) used everywhere in the codebase.
  - `mux`: A few useful abstractions for multiplexing.
  - `janet`: A library for Janet/Go interoperation.
  - `emu`: A vt100 terminal emulator.
  - `fuzzy`: A [fuzzy finder](./user-input/fuzzy-finding.md).
  - `replay`: A terminal session player, otherwise known as [replay mode](./replay-mode.md).
  - `taro`: A fork of [charmbracelet/bubbletea](https://github.com/charmbracelet/bubbletea) adapted for use in `cy`'s windowing abstraction (described [below](./architecture.md#screens-and-streams).)
- `docs`: Contains all of `cy`'s documentation. `cy` uses [mdbook](https://github.com/rust-lang/mdBook) to build the documentation site.

## Screens and streams

The two most important abstractions in `cy`'s codebase are [`Screen`s](https://github.com/cfoust/cy/blob/main/pkg/mux/module.go?plain=1#L42) and [`Stream`s](https://github.com/cfoust/cy/blob/main/pkg/mux/module.go#L36), which are defined in the [mux](https://github.com/cfoust/cy/tree/main/pkg/mux) package.

### Stream

A `Stream` is just a resizable (this is important!) bidirectional stream of bytes that can be read from and written to. As of writing, it looks like this:

```go
type Stream interface {
    Read(p []byte) (n int, err error)
    Write(p []byte) (n int, err error)
    Resize(size Vec2) error
}
```

From the perspective of the process you're running, this interface concisely describes the functionality of your terminal emulator (e.g. xterm, kitty.) Typing into your terminal writes to the process; any output it produces is read and interpreted in a predictable, standard way (the VT100 quasi-standard.) Resizing your terminal sends a resize event, `SIGWINCH`, which the process can react to.

This is useful because you can represent lots of things as a `Stream`:

1. [Pseudo-terminals](https://en.wikipedia.org/wiki/Pseudoterminal): By connecting a process to a pseudo-terminal, it behaves as though a user had run it interactively.
   - `Write`: Writes are written directly to that process' standard input.
   - `Read`: Reads correspond to whatever that process writes to standard output.
   - `Resize`: Will set the size of the pseudo-terminal (and thus send `SIGWINCH` to the process).
2. Clients: `cy` clients that connect to the server can be written to and read from.
   - `Write`: Writes are interpreted as user input, typically sequences of keys.
   - `Read`: Reads consist of the shortest sequence of bytes necessary to update the client's terminal to match `cy`'s understanding of that client's screen.
   - `Resize`: Resizing a client indicates to `cy` that it should resize everything on that client's screen and redraw accordingly.

Streams can be composed and form pipes of arbitrary complexity. For example, `cy` records terminal sessions by proxying a `Stream` (sort of like `tee`.)

However, for a terminal multiplexer this is clearly not enough. A `Stream` is stateless. In other words, there is no way to know what the state of the terminal that is attached to that `Stream`. That's where `Screen`s come in.

### Screen

A [`Screen`](https://github.com/cfoust/cy/blob/main/pkg/mux/module.go?plain=1#L42) can be thought of, conceptually, as an application to which you can send events (such as user input) and receive any updates it produces (such as changes to the screen's contents).

The state of a screen (represented in the `cy` codebase as a [`tty.State`](https://github.com/cfoust/cy/blob/main/pkg/geom/tty/module.go?plain=1#L9)) is identical to that of a terminal emulator:

- A two-dimensional buffer of Unicode characters
- The state of the cursor including its position and style

A [**pane**](./groups-and-panes.md#panes), described elsewhere, is a good example of a `Screen`.

If that all sounds abstract, the interface for `Screen` looks like this:

```go
type Screen interface {
    // State gets the current visual state of the Screen.
    State() *tty.State

    // Resize adjusts the screen to fit `size`.
    Resize(size Vec2) error

    // Subscribe subscribes to any updates to the screen, which are usually
    // caused by changes to the screen's state.
    Subscribe(context.Context) *Updater

    // Send sends a message to the Screen.
    Send(message interface{})
}
```

`Send` looks scary, but it's used in `cy` mostly for key and mouse events.

The easiest way to understand this is to think of a `Screen` as something that can render a `Stream` and turn it into something that can be composed with other `Screen`s. In fact, there is a `Screen` that [does just that](https://github.com/cfoust/cy/blob/main/pkg/mux/screen/terminal.go?plain=1#L13).

`cy`'s [fuzzy finder](./user-input/fuzzy-finding.md) and [replay mode](./replay-mode.md) are both just `Screen`s, albeit complicated ones.

Some `Screen`s just exist to compose other screens in some way, which is the bread and butter of any terminal multiplexer.

The simplest example of this is `cy`'s [`Layers`](https://github.com/cfoust/cy/blob/main/pkg/mux/screen/layers.go?plain=1#L22), a `Screen` that lets you render one or more `Screen`s on top of one another, letting the screens underneath show through if any cells of the layer above are transparent.

`Layers` is used to place the pane the user is currently interacting with on top of a [frame](./frames.md), such as in the default viewport:

{{story png placeholder}}

It is also used for `cy`'s toast messages ({{api msg/toast}}), which are implemented using a noninteractive `Screen` that is layered over the rest of the content on the client's screen.

### Tying it all together

To illustrate the difference between `Screen`s and `Streams`, consider the following description of how data flows back and forth from a client to its `Screen`s and back again.

The flow for client input works like this:

1. The client presses a key in the terminal where they originally connected to `cy`. The terminal emulator writes the byte sequence for that key to the standard input of the process controlling the terminal, which in this case is `cy` running as a client.
   - When `cy` is running in client mode, it represents its connection to the server with a `Stream` ([`ClientIO`](https://github.com/cfoust/cy/blob/main/cmd/cy/client.go?plain=1#L49)), the `Read`, `Write`, and `Resize` methods of which are [connected](https://github.com/cfoust/cy/blob/main/pkg/mux/stream/cli/module.go?plain=1#L20) directly to the standard output, standard input, and `SIGWINCH` events of the controlling terminal.
2. All of the events are sent using the WebSocket protocol via a Unix socket to the `cy` server, which is a separate process.
3. The `cy` server writes the incoming bytes it received from the client to the corresponding [`Client`](https://github.com/cfoust/cy/blob/main/pkg/cy/client.go?plain=1#L34) on the server. A `Client` is just a `Stream`.
4. The `Client` translates the bytes into key and mouse events that are then sent (via `Send`) to the `Screen` the `Client` is attached to. These events usually travel through several different `Screen`s before reaching their destination, but ultimately they are passed into whatever `Screen` the client is currently attached to--whether that be a pane, the fuzzy finder, or replay mode.

The flow for client output is somewhat simpler:

1. Whenever the `Screen` the `Client` is attached to changes in some way (in other words, it produces an event that is published to its subscribers via `Subscribe`).
2. The client's [`Renderer`](https://github.com/cfoust/cy/blob/main/pkg/mux/stream/renderer/module.go?plain=1#L24) receives this event and calls `State()` on the client's `Screen`, which produces a `tty.State`. The `Renderer` then calculates the sequence of bytes necessary to transform the actual client's terminal screen to match the `cy` server's state.
3. This byte string is sent via the aforementioned WebSocket connection.
4. It is ultimately `Read` by the user's terminal and written to standard output, thus triggering the visual changes the user expects.
