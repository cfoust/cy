# Roadmap

This document outlines the features I plan on implementing in the next few months. Features denoted with "\*" are tentative and may not make the cut.

- [ ] **Performance**: cy is not slow, but there's still room for improvement in rendering speed.
  - [ ] **Smarter rendering algorithm:** `cy` uses a "damage" algorithm to detect what parts of the screen have changed and only rerender those portions. This is intended to minimize the burden on the client's terminal emulator. The current version is inefficient, since the algorithm still emits escape sequences for styles for every cell, even if adjacent cells have the same styling.
  - [ ] **Automated performance measurement:** In addition to testing functionality, cy should have a system for measuring the performance of [Screens](/architecture.md#screens-and-streams). This could happen in CI as well.
  - [ ] **Benchmarking against other terminal multiplexers:** It would be simple enough to design tests for checking cy's performance against other commonly used multiplexers.
- [ ] **Plugin system**: Ideally this would be nothing more than something like [vim-plug](https://github.com/junegunn/vim-plug) or [lazy.nvim](https://github.com/folke/lazy.nvim), but there are several things we need in order for this to be possible.
  - [ ] **API functions for executing external commands**
  - [ ] **Progress bars\*:** It would be nice for Janet code to be able to show the user the progress of some long-running operation.
  - [ ] **tmux emulation:** This would be the first proof-of-concept cy plugin. Some people really like tmux, and I intentionally built cy's layout functionality so that it would be flexible enough to emulate tmux's user interface. This does not have to be that comprehensive, it could just mimic most of tmux's common keybindings.
- [ ] **Persistent parameter store:** This would add another target for {{api param/set}} called `:persist` (name pending) which would be a key-value store backed by an SQLite database stored in `$XDG_STATE_HOME`. Any serializable Janet value could be written to this parameter store. This persistent parameter store could be used for things like saving and restoring layouts from previous sessions, reopening frequently used projects, et cetera.
- [ ] **fzf-cy\*:** `cy` literally uses `fzf`'s algorithm and its fuzzy finder should be able to be used as a drop-in replacement for `fzf` just like in [fzf-tmux](https://github.com/junegunn/fzf/blob/master/bin/fzf-tmux). In other words, `cy`'s fuzzy finder should support everything (within reason) that `fzf` does.
