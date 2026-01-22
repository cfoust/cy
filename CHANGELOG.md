# Changelog

## 1.10.1 - 2026-01-22

- A pile of fixes for the kitty keyboard protocol

## 1.10.0 - 2026-01-18

- Significant improvements in memory usage: `.borg` files are now loaded on demand in replay mode
- Command detection now uses [OSC-133 escape sequences](https://gitlab.freedesktop.org/Per_Bothner/specifications/blob/master/proposals/semantic-prompts.md).
- New parameter: `:animation-fps` to limit animation FPS
- Fix: `(pane/current)` reports the correct pane when invoked from `cy exec`
- Support for [bracketed paste](https://en.wikipedia.org/wiki/Bracketed-paste)
- Fix for a range of poorly-handled key sequences

## 1.9.1 - 2025-12-28

- Fix for copying to clipboard using OSC-52

## 1.9.0 - 2025-12-27

- Add full support for the [kitty keyboard protocol](https://sw.kovidgoyal.net/kitty/keyboard-protocol/#disambiguate-escape-codes)
- Clean up of most existing animations
- New parameter: `:animate-delay` to delay the beginning of all animations

## 1.8.0 - 2025-11-15

- Matched cells are now highlighted in `(input/find)` searches
- Add a thumbs pattern for matching Git stashes
- New frame: `grid`
- Remove `CLAUDE.md`: AI bubble pop imminent

## 1.7.0 - 2025-08-11

- Use OSC-52 sequences for `(clipboard/set)`
- Mouse support in copy mode
- New `:persist` target for `(param/get)` and `(param/set)`, which is a key-value store stored in `$XDG_STATE_HOME`
- New API function: `(cmd/title)` for getting the title of a pane
- `{{param}}` syntax in documentation site
- Upgrade to Go 1.24.0

## 1.6.1 - 2025-08-07

- Fix goofy regex behavior

## 1.6.0 - 2025-08-07

- New thumbs mode for quickly inserting text patterns
- Support for `:single` mode in `(input/text)` and new `(input/ok?)` function
- Refactored layout engine
- Fix for certain ncurses applications

## 1.5.1 - 2025-05-11

- Quick fix for old regression in `:margins` layout nodes

## 1.5.0 - 2025-05-11

- Unify the search bars for time and copy mode inside of replay

## 1.4.1 - 2025-04-03

- Two new animations: `city` and `perlin`

## 1.4.0 - 2025-04-03

- Software 3D rasterizer for animations

## 1.3.1 - 2024-12-01

- Fix cursor location on long lines
- Support `alt-` key specifiers
- Documentation updates

## 1.3.0 - 2024-11-16

- Fix for server startup locking issue

## 1.2.0 - 2024-11-08

- Registers: copy text in replay mode to and from the system clipboard and any one of 62 built-in registers (a la vim)
- System clipboard integration with `(clipboard/set)` and `(clipboard/get)`

## 1.1.0 - 2024-10-20

- Slime animation
- New layout node: `:color-map`, which applies a color scheme to a layout node

## 1.0.0 - 2024-10-03

- Color map system (color schemes)
- Pane management improvements (kill without removing)

## 0.12.0 - 2024-09-22

- Adds a range of default parameters for theming cy

## 0.11.0 - 2024-09-18

- Command history system for storing and retrieving executed commands

## 0.10.0 - 2024-09-06

- Search mode, which permits searching across recorded `.borg` files

## 0.9.0 - 2024-08-22

- `cy exec`, which you can use to control cy programmatically from other processes, such as shell scripts
- `cy recall`, which lets you recall the output of a command you've already run

## 0.8.0 - 2024-08-17

- New layout node: `:bar`, which lets you make tmux-like status lines

## 0.7.0 - 2024-08-10

- New layout node: `:tabs`

## 0.6.0 - 2024-08-08

- API functions for styling text for use in layouts or other parts of the UI
- Convenience functions for creating layouts

## 0.5.0 - 2024-08-03

- New layout node: `:borders`

## 0.4.0 - 2024-08-01

- Declarative layout system

## 0.2.5 - 2024-07-19

- `(input/text)` for freeform text input

## 0.1.19 - 2024-07-07

- New animation: `fluid`

## 0.1.13 - 2024-06-16

- Incremental search in copy mode
- vim motions in copy mode

## 0.1.11 - 2024-02-09

- Support for fuzzy-finding tabular data with `(input/find)`

## 0.1.5 - 2023-11-02

- Replay mode
- Splash screen
- Toast notifications

## 0.1.0 - 2023-09-15

- Core multiplexer functionality
- Janet integration
- Session recording
- Tree-based pane management
