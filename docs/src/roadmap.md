# Roadmap

> Note (2025-08-12): This page is out of date and does not represent current priorities.

This document outlines the features I plan on implementing in the next few months. Features denoted with "\*" are tentative and may not make the cut.

- [ ] **Performance**: cy is not slow, but there's still room for improvement in rendering speed.
  - [ ] **Smarter rendering algorithm:** `cy` uses a "damage" algorithm to detect what parts of the screen have changed and only rerender those portions. This is intended to minimize the burden on the client's terminal emulator. The current version is inefficient, since the algorithm still emits escape sequences for styles for every cell, even if adjacent cells have the same styling.
  - [ ] **Automated performance measurement:** In addition to testing functionality, cy should have a system for measuring the performance of [Screens](/architecture.md#screens-and-streams). This could happen in CI as well.
  - [ ] **Benchmarking against other terminal multiplexers:** It would be simple enough to design tests for checking cy's performance against other commonly used multiplexers.
- [ ] **Plugin system**: Ideally this would be nothing more than something like [vim-plug](https://github.com/junegunn/vim-plug) or [lazy.nvim](https://github.com/folke/lazy.nvim), but there are several things we need in order for this to be possible.
  - [ ] **API functions for executing external commands**
  - [ ] **Progress bars\*:** It would be nice for Janet code to be able to show the user the progress of some long-running operation.
  - [ ] **tmux emulation:** This would be the first proof-of-concept cy plugin. Some people really like tmux, and I intentionally built cy's layout functionality so that it would be flexible enough to emulate tmux's user interface. This does not have to be that comprehensive, it could just mimic most of tmux's common keybindings.
- [ ] **cy-find\*:** `cy`'s fuzzy finder should be able to be used as a standalone command-line tool, similar to other fuzzy finders. This would allow users to leverage cy's fast fuzzy matching in scripts and workflows.
