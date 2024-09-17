# Command history

One of `cy`'s distinguishing features is that it can detect the commands that you run. It also records:

- When the command was executed
- When it finished executing
- All of the output the command produced (by indexing into [.borg files](/replay-mode.md#recording-to-disk))
- The directory in which the command was run

This metadata is stored in an SQLite database called `cmd.db` (in your [:data-directory](/default-parameters.md#data-directory)) and accessible in Janet via the {{api cmd/query}} API.

It uses this information to provide a range of functionality:

- [A replacement for <kbd>ctrl+r</kbd>](/command-history/ctrl+r.md) that lets you see the context of all of the commands you have ever run
- [Quick shortcuts for jumping to the output of a command](/command-history/switching-panes.md), both in an existing shell session and one that was previously recorded
- Additional [features in replay mode](/command-history/replay-mode.md) for jumping between commands

## Installation

To enable command detection, you must add a special escape sequence to your prompt in the shell of your choice:

```
\033Pcy\033\\
```

This is a custom [device control sequence](https://www.vt100.net/emu/dec_ansi_parser): `ESC P cy ESC \`. It is a type of escape sequence that is seldom used by modern programs and is ignored by terminal emulators, so it will have no effect even when you are not using your shell inside of `cy`.

There are instructions for how to configure this correctly in popular shells below.

<details>
<summary>bash</summary>

```bash
# Place this anywhere in the PS1 variable:
\[\033Pcy\033\\\]

# For example:
PS1='\[\033Pcy\033\\\] ▸▸'
```

You can put it anywhere; its position does not matter and it does not contain any printable characters.

</details>

<details>
<summary>zsh</summary>

```zsh
# Place this anywhere in the PROMPT variable:
%{\033Pcy\033\\%}

# For example:
PROMPT=$'%{\033Pcy\033\\%} >'
```

</details>

<details>
<summary>fish</summary>

Put this somewhere in your `fish_prompt` or just add `\033Pcy\033\\` to any existing string that's already there.

```fish
printf '\033Pcy\033\\'
```

</details>

### Directory detection

By default, `cy` detects the directory in which a command is executed by regularly getting the working directory of the process running in your terminal (such as a shell.) This produces incorrect results in cases like `ssh`, where the actual directory is distinct from the one reported by examining the process.

To address this, `cy` supports the [OSC-7](https://gitlab.freedesktop.org/terminal-wg/specifications/-/issues/20) quasi-standard. OSC-7 is a custom escape sequence that allows a program to indicate its current directory to the terminal emulator.

You must explicitly configure your shell (or any other program) to explicitly emit OSC-7 sequences. You can find instructions for doing so [here](https://codeberg.org/dnkl/foot/wiki#shell-integration).

## How does it work?

> You can skip this section.

A terminal multiplexer is like a proxy: it is an intermediary between your actual terminal and one or more virtual terminals that it controls. It also responsible for passing the output a program (such as a shell) produces to its corresponding terminal. This control means that your terminal multiplexer can do whatever calculations it wants on that output, including (in `cy`'s case) recording it.

`cy` uses this ability to observe the state of the terminal before and after interpreting a particular output of your program. A terminal emulator is just a state machine that interprets a stream of bytes and adjusts the state of the screen, which is just a grid of cells containing Unicode characters, accordingly. Therefore `cy` can detect the exact changes a byte string made to your screen.

When `cy` detects the special escape sequence above, it applies a special "flag" to the screen cells that particular byte string produced. In doing so, it can know which screen cells (and therefore, which writes) correspond to your shell's prompt and which do not.

Almost invariably, shell sessions have the following structure:

```
PROMPT [user command]
OUTPUT
PROMPT
```

The commands a user enters manually follow a consistent pattern (they always follow a prompt) and so once that step is done, all that's left is to collect the output a command produced before the next prompt and voilà.
