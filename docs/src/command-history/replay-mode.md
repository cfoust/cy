# Replay mode

## Time mode

You can quickly jump back to the instant in time _just before a command began executing_ using the {{bind :time [ c}} and {{bind :time ] c}} bindings.

{{story cast replay/command/time-jump}}

## Copy mode

When the cursor is on the output of a command in copy mode, the command itself is shown in replay mode's status bar at the bottom of the screen.

Just like in time mode, you can also jump between commands using {{bind :copy [ c}} and {{bind :copy ] c}}.

{{story cast replay/command/copy-jump}}

You can also quickly select the complete output of a command using {{bind :copy [ C}} and {{bind :copy ] C}}.

{{story cast replay/command/copy-jump-and-copy}}
