# Command history

One of `cy`'s distinguishing features is that it can detect the commands that you run. It also records:

- When the command was executed
- When it finished executing
- All of the output the command produced (by indexing into [.borg files](/replay-mode.md#recording-to-disk))
- The directory in which the command was run

This metadata is stored in an SQLite database called `cmd.db` (in your [:data-directory](/default-parameters.md#data-directory)) and accessible in Janet via the {{api cmd/query}} API.

`cy` uses this information to provide a range of functionality:

- [A replacement for <kbd>ctrl+r</kbd>](/command-history/ctrl+r.md) that lets you see the context of a command when it was executed
- [Quick shortcuts for switching panes](/command-history/switching-panes.md) based on the commands executed in them
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

You must explicitly configure your shell (or any other program) to emit OSC-7 sequences. You can find instructions for doing so [here](https://codeberg.org/dnkl/foot/wiki#shell-integration). `cy` does not do any special processing of the string you emit using OSC-7: it need not be a valid directory.

## How does it work?

A terminal multiplexer is like a proxy: it is an intermediary between your actual terminal and one or more virtual terminals that it controls. It also responsible for passing the output a program (such as a shell) produces to its corresponding terminal. This control means that your terminal multiplexer can do whatever calculations it wants on that output, including (in `cy`'s case) recording it.

`cy` uses this ability to observe the state of the terminal before and after interpreting a particular output of the underlying process (otherwise known as a write made to standard output or error).

A terminal emulator is just a state machine that interprets a stream of bytes and adjusts the state of the screen accordingly. The screen is a grid of cells, each of which contains a Unicode character and has other styling information.

As a result, `cy` can detect the exact changes a byte string made to your screen. When `cy` detects the special escape sequence above, it applies a flag to the screen cells that particular output produced--in this case, all of the cells comprising your shell prompt. By doing so, it can determine which screen cells (and therefore, which writes) correspond to the prompt and which do not.

Almost invariably, shell sessions follow the following pattern:

```
PROMPT [user command]
OUTPUT
PROMPT
```

The commands a user enters manually also have a consistent structure (they always follow a prompt) and so once that step is done, all that's left is to collect the output a command produced between prompts and voilà.

This approach has downsides. One is that `cy` is unable to distinguish between the output produced when running multiple processes at once, such as with Bash's backgrounding functionality (i.e. `&`.)

