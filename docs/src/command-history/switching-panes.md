# Switching panes

`cy`'s default configuration also defines a few actions that use command detection to do interesting things:

- {{api action/jump-command}} ({{bind :root ctrl+a C}}): Choose from a list of all commands and jump to the location of that command in its pane's scrollback history.
- {{api action/jump-pane-command}} ({{bind :root ctrl+a c}}): Choose from a list of all of the commands run since the `cy` server started and jump to the pane where that command was run.
