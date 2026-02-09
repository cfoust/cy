# doc: Find

(input/find inputs &named prompt full reverse animated headers)

`(input/find)` is a general-purpose fuzzy finder. When invoked, it prompts the user to choose from one of the items provided in `inputs`. `(input/find)` does not return until the user makes a choice; if they choose nothing (such as by hitting <kbd>ctrl+c</kbd>), it returns `nil`.

`inputs` is an array with elements that can take different forms depending on the desired behavior. See more on the [page about fuzzy finding](/user-input/fuzzy-finding.md).

This function supports a range of named parameters that adjust its functionality:

- `:animated` (boolean): Enable and disable background animation.
- `:case-sensitive` (boolean): Whether the matching algorithm should respect differences in case. The default is `false`.
- `:full` (boolean): If true, occupy the entire screen.
- `:headers` ([]string): Provide a title for each column. This is mostly used for filtering tabular data.
- `:prompt` (string): The text that will be shown beneath the search window.
- `:reverse` (boolean): Display from the top of the screen (rather than the bottom.)
- `:width` (int): Set the width of the match window (if not in full screen mode.)
- `:height` (int): Set the maximum height of the match window. This applies both in full screen and floating mode.

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

(input/thumbs &named alphabet regexp)

`(input/thumbs)` searches the contents of the screen for matches of a set of patterns and prompts the user to select one using short sequences of characters. The return value of the function is the match text or `nil` if the user did not select anything. This is similar to the functionality offered by [tmux-thumbs](https://github.com/fcsonline/tmux-thumbs) or [tmux-fingers](https://github.com/Morantron/tmux-fingers/). Unlike those programs, `(input/thumbs)` searches across the entire screen and is sensitive to pane boundaries, so matches that occur in wrapped lines will still appear.

This function supports the following named parameters:

- `:alphabet` (string): Set the character set used for generating hints. Default is `"asdfqwerzxcvjklmiuopghtybn"`.
- `:patterns` ([]string): Array of regular expressions to search for.

#### Patterns

Regular expressions supplied to the `:patterns` parameter have special behavior for effectively matching content on the screen:
* If a pattern has a named capture group called `match`, the content matched by that group will be used as the match (as opposed to any other matched group.)
    - Example (filenames): `(?P<match>([.\w\-@$~\[\]]+)?(/[.\w\-@$\[\]]+)+)`
* If a pattern has several capture groups, a match of the full pattern will produce several matches.
    - Example (git diffs): `diff --git a/([.\w\-@~\[\]]+?/[.\w\-@\[\]]+) b/([.\w\-@~\[\]]+?/[.\w\-@\[\]]+)`
    - If this pattern matches, hints will appear over the text following `a/` and `b/`

# doc: ThumbsDefaultPatterns

(input/thumbs/default-patterns)

Get the default search patterns used by {{api input/thumbs}}. This is useful for extending the default patterns without replacing them.
