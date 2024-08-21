# Command detection

`cy` can detect the commands you run and the output they produce. It does this using a special string that you put in your shell's prompt that lets it determine where the commands you enter begin and end.

By enabling this feature, you gain access to a range of functionality for jumping between panes, copying command output, and much more.

## Enabling command detection

All you need to do for `cy` to be able to detect your commands is add a special escape sequence to your prompt, depending on the shell you use. You can mix and match shells, even within a single terminal session, and `cy` will not get confused.

### bash

```bash
# Place this anywhere in the PS1 variable:
\[\033Pcy\033\\\]

# For example:
PS1='\[\033Pcy\033\\\] ▸▸'
```

You can put it anywhere; its position does not matter and it does not contain any printable characters.

### zsh

Similar to Bash, you will need to

```zsh
# Place this anywhere in the PROMPT variable:
%{\033Pcy\033\\%}

# For example:
PROMPT=$'%{\033Pcy\033\\%} >'
```

### fish

Put this somewhere in your `fish_prompt` or just add `\033Pcy\033\\` to any existing string that's already there.

```fish
printf '\033Pcy\033\\'
```

## Features

### Replay mode

Enabling command detection adds additional features to replay mode both in time mode and copy mode.

#### Time mode

You can quickly jump back to the instant in time _just before a command began executing_ using the {{bind :time [ c}} and {{bind :time ] c}} bindings.

{{story cast replay/command/time-jump}}

#### Copy mode

When the cursor is on the output of a command in copy mode, the command itself is shown in replay mode's status bar at the bottom of the screen.

Just like in time mode, you can also jump between commands using {{bind :copy [ c}} and {{bind :copy ] c}}.

{{story cast replay/command/copy-jump}}

You can also quickly select the complete output of a command using {{bind :copy [ C}} and {{bind :copy ] C}}.

{{story cast replay/command/copy-jump-and-copy}}

### Switching panes

`cy`'s default configuration also defines a few actions that use command detection to do interesting things:

* {{api action/jump-command}} ({{bind :root ctrl+a C}}): Choose from a list of all commands and jump to the location of that command in its pane's scrollback history.
* {{api action/jump-pane-command}} ({{bind :root ctrl+a c}}): Choose from a list of all of the commands run since the `cy` server started and jump to the pane where that command was run.

### Recall

[`cy recall`](/cli.md#recall) only works if command detection is enabled.
