# Architecture

This document is intended to be a brief introduction to `cy`'s code structure and its commonly used abstractions. The intended audience is anyone interested in contributing to `cy` or any of its constituent libraries, some of which may (eventually) be broken out into separate projects.

It is safe to assume that the high-level description in this document will remain reliable despite changes in the actual implementation, but if you are ever in doubt:

1. Read the README for the package you are modifying. Most packages in `pkg` have their own READMEs (along with some sub-packages.)
2. Ask for help [in Discord](https://discord.gg/NRQG3wbWGM).
3. Consult the code itself.

`cy` is written in Go and Janet. I chose Go because I had written other projects with strong concurrency needs and it seemed like a natural fit. Janet is a Lisp-like scripting language that I chose because it sounded like fun.

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
  - `fuzzy`: A [fuzzy finder](./fuzzy-finding.md).
  - `replay`: A terminal session player, otherwise known as [replay mode](./replay-mode.md).
  - `taro`: A fork of [charmbracelet/bubbletea](https://github.com/charmbracelet/bubbletea) adapted for use in `cy`'s windowing abstraction (described [below](./architecture.md#screens-and-streams).)
- `docs`: Contains all of `cy`'s documentation. `cy` uses [mdbook](https://github.com/rust-lang/mdBook) to build the documentation site.

## Screens and streams

The two most important abstractions in `cy`'s codebase are [`Screens`](https://github.com/cfoust/cy/blob/main/pkg/mux/module.go?plain=1#L42) and [`Streams`](https://github.com/cfoust/cy/blob/main/pkg/mux/module.go#L36), which are defined in the [mux](https://github.com/cfoust/cy/tree/main/pkg/mux) package.

A `Stream` is just a resizable (this is important!) stream of bytes that can be read from and written to. As of writing, it looks like this:

```go
// Note: this was changed from the actual implementation for simplicity.
type Stream interface {
    io.ReadWriter
    Resize(size Vec2) error
}
```

From the perspective of the process you're running, this interface concisely describes the functionality of your terminal emulator (e.g. xterm, kitty.) Typing into your terminal writes to the process; any output it produces is read and interpreted in a predictable, standard way (the VT100 quasi-standard.) Resizing your terminal sends a resize event, `SIGWINCH`, which the process can react to.
