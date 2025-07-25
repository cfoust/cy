# doc: Find

(input/find inputs &named prompt full reverse animated headers)

`(input/find)` is a general-purpose fuzzy finder that is similar to `fzf`. When invoked, it prompts the user to choose from one of the items provided in `inputs`. `(input/find)` does not return until the user makes a choice; if they choose nothing (such as by hitting <kbd>ctrl+c</kbd>), it returns `nil`.

`inputs` is an array with elements that can take different forms depending on the desired behavior. See more on the [page about fuzzy finding](/user-input/fuzzy-finding.md).

This function supports a range of named parameters that adjust its functionality:

- `:animated` (boolean): Enable and disable background animation.
- `:case-sensitive` (boolean): Whether the matching algorithm should respect differences in case. The default is `false`.
- `:full` (boolean): If true, occupy the entire screen.
- `:headers` ([]string): Provide a title for each column. This mostly used for filtering tabular data.
- `:prompt` (string): The text that will be shown beneath the search window.
- `:reverse` (boolean): Display from the top of the screen (rather than the bottom.)
- `:width` (int): Set the width of the match window (if not in full screen mode.)
- `:height` (int): Set the maximum height of the match window. This applies bth in full screen and floating mode.

# doc: Text

(input/text prompt &named preset placeholder full reverse animated single)

`(input/text)` prompts the user with a freeform text input. If the input is non-empty when the user presses enter, `(input/text)` returns the value of the text input; otherwise it returns `nil`.

The only required parameter, `prompt`, is a string that determines the prompt text that is shown below the text input.

This function supports a range of named parameters that adjust its functionality:

- `:preset` (string): Pre-fill the value of the text input.
- `:placeholder` (string): This string will be shown when the text input is empty.
- `:full` (boolean): If true, occupy the entire screen.
- `:reverse` (boolean): Display from the top of the screen (rather than the bottom.)
- `:animated` (boolean): Enable and disable background animation.
- `:single` (boolean): If true, accept only a single character and return immediately when any printable character is typed, without requiring the user to press Enter.

# doc: Thumbs

(input/thumbs &named animated alphabet regexp)

`(input/thumbs)` provides tmux-thumbs-like functionality for quickly selecting and copying text patterns from the current terminal screen. When invoked, it scans the visible screen content for predefined patterns (URLs, file paths, git SHAs, etc.) and overlays short hint sequences on each match. The user can then type a hint sequence to select and return the corresponding matched text.

This function captures the current screen content and searches for matches using a comprehensive set of regular expressions. Each match is assigned a unique hint (like "a", "s", "d", etc.) that appears as a yellow overlay on the screen. Users type the hint characters to select a match, and the function returns the matched text.

## Default Patterns

`(input/thumbs)` automatically detects the following patterns:

- **URLs**: `https://`, `http://`, `git@`, `git://`, `ssh://`, `ftp://`, `file://`
- **File paths**: Absolute and relative paths like `/var/log/file.log`, `./config.yaml`  
- **Git SHAs**: Short and long git commit hashes
- **Git diff files**: Paths in `diff --git`, `--- a/`, `+++ b/` lines
- **IP addresses**: IPv4 (`192.168.1.1`) and IPv6 (`fe80::1`) addresses  
- **UUIDs**: Standard UUID format `123e4567-e89b-12d3-a456-426655440000`
- **Colors**: Hex colors like `#ff0000`, `#00ff00`
- **Docker images**: SHA256 hashes in docker output
- **Numbers**: 4+ digit numbers like `1234`, `5678`
- **Addresses**: Hexadecimal addresses like `0x1234abcd`
- **Markdown URLs**: Links in `[text](url)` format
- **IPFS hashes**: Content IDs starting with `Qm`

## Usage

```janet
# Basic usage - find all patterns on current screen
(def selected (input/thumbs))

# Add custom patterns
(def email (input/thumbs :regexp ["[\\w.-]+@[\\w.-]+\\.[\\w]+"]))

# Use numeric hints instead of letters  
(def path (input/thumbs :alphabet "123456789"))

# Disable background animation
(def url (input/thumbs :animated false))
```

## Parameters

This function supports the following named parameters:

- `:animated` (boolean): Enable or disable background animation. Default is `true`.
- `:alphabet` (string): Set the character set used for generating hints. Default is `"asdfqwerzxcvjklmiuopghtybn"` (qwerty home row optimized). Other useful alphabets include `"123456789"` for numeric hints or `"abcdefghijklmnopqrstuvwxyz"` for alphabetical order.
- `:regexp` ([]string): Array of additional regular expressions to search for. These custom patterns take precedence over default patterns and will be matched first.

## Interaction

- **Type hint characters**: Enter the yellow hint letters/numbers to select a match
- **Progressive selection**: As you type, invalid hints are filtered out  
- **Enter**: Confirm selection with current partial input (if it matches a hint)
- **Escape/Ctrl+C**: Cancel and return `nil`

The function returns the matched text as a string, or `nil` if cancelled.

## Examples

```janet
# Select any URL from the screen
(def url (input/thumbs))
(if url (shell/run "open" url))

# Find email addresses with custom pattern
(def email (input/thumbs :regexp ["[\\w.-]+@[\\w.-]+\\.[\\w]+"]))
(if email (clipboard/set email))

# Use with numeric hints for easier typing  
(def file-path (input/thumbs :alphabet "123456789"))
(if file-path (edit file-path))

# Combine with other cy functions
(def commit-sha (input/thumbs))
(if commit-sha (shell/run "git" "show" commit-sha))
```
