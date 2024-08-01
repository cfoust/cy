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

# doc: Text

(input/text prompt &named preset placeholder full reverse animated)

`(input/text)` prompts the user with a freeform text input. If the input is non-empty when the user presses enter, `(input/text)` returns the value of the text input; otherwise it returns `nil`.

The only required parameter, `prompt`, is a string that determines the prompt text that is shown below the text input.

This function supports a range of named parameters that adjust its functionality:

- `:preset` (string): Pre-fill the value of the text input.
- `:placeholder` (string): This string will be shown when the text input is empty.
- `:full` (boolean): If true, occupy the entire screen.
- `:reverse` (boolean): Display from the top of the screen (rather than the bottom.)
- `:animated` (boolean): Enable and disable background animation.
