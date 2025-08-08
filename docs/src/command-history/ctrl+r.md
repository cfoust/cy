# ctrl+r

Hitting {{bind :root ctrl+a ctrl+r}} allows you to choose a command from your command history and insert it into your current pane. This works just like [fzf](https://github.com/junegunn/fzf), [atuin](https://atuin.sh/), [hstr](https://github.com/dvorka/hstr), et al, but it also gives you a preview of what the screen looked like when you executed that command.

You can also use the same interface to open a particular command from your history in [replay mode](/replay-mode.md) using the {{api action/jump-history-command}} action. In effect, you can jump back to the exact moment in time you executed any command.

### Why not bind this directly in the shell?

Most programs that replace the default <kbd>ctrl+r</kbd> functionality require you to overwrite the existing key binding, typically through per-shell configurations. The advantage of doing this at the level of the terminal multiplexer (as opposed to configuring individual shells) is that `cy`'s <kbd>ctrl+r</kbd> works anywhere: in editors, over SSH, et cetera. It does not matter what system you're on, you can still just hit {{bind :root ctrl+a ctrl+r}} and insert a command from your history.

Another subtle advantage is that once you've put `cy`'s command detection sequence in your prompt on a system, you will never need to update it. In the example of a remote system, `cy` need not even be installed; when you connect from `cy`, command detection will work even if the remote does not have `cy`.
