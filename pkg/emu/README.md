# emu (formerly vt10x)

Package emu provides a VT100-compatible terminal emulator. For the most part it attempts to emulate xterm as closely as possible (ie to be used with `TERM=xterm-256color.`)

emu's basic mode of operation is quite simple: you `Write()` some bytes and it will correctly calculate the state of the virtual terminal which you can happily capture and send elsewhere (with `Terminal.View()`).

emu's magic, however, comes from `Terminal.Flow()`, which is an API for viewing the terminal's scrollback buffer _with a viewport of arbitrary size_. This is important because `cy`'s core feature is to be able to replay terminal sessions, the lines of which should wrap appropriately to fit your terminal screen.

This package is a fork of [github.com/hinshun/vt10x](https://github.com/hinshun/vt10x). The original library was rough, incomplete, and had a range of serious bugs that I discovered after integrating it. Because of this, little of the original code remains.
