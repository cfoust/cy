# Command history

One of `cy`'s distinguishing features is that it detects the commands that you run. It also records:

- When the command was executed
- When it finished executing
- All of the output the command produced (both standard output and standard error)
- The directory in which the command was run

It uses this information to provide a range of functionality:

- [A replacement for <kbd>ctrl+r</kbd>](/command-history/ctrl+r.md) that lets you see the context of all of the commands you have ever run
- [Quick shortcuts for jumping to the output of a command](/command-history/switching-panes.md), both in an existing shell session and one that was previously recorded
- Additional [features in replay mode](/command-history/replay-mode.md) for jumping between commands

## Installation

All you need to do for `cy` to be able to detect your commands is add a special escape sequence to your prompt in the shell of your choice. You can mix and match shells, even within a single terminal session, and `cy` will not get confused.

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
