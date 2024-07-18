# Command detection

`cy` can detect the commands you run and the output they produce. It does this by ~using magic~ having you put a special string in your shell's prompt that lets it determine where the commands you enter begin and end.

By enabling this feature, you gain access to a bunch of cool functionality for jumping between panes, copying command output, and much more.

## Enabling command detection

All you need to do for `cy` to be able to detect your commands is add a special escape sequence to your prompt, depending on the shell you use. Note that you can mix and match, even within a single terminal session, and `cy` will not get confused.

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

## Command functionality


