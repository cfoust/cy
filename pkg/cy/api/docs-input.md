# doc: Find

(input/find inputs &named prompt full reverse animated)

`(input/find)` is a general-purpose fuzzy finder that is similar to `fzf`. When invoked, it prompts the user to choose from one of the items provided in `inputs`. `(input/find)` does not return until the user makes a choice; if they choose nothing (such as by hitting `ctrl+c`), it returns `nil`.

`inputs` is an array with elements that can take different forms depending on the desired behavior. See more on the [page about fuzzy finding](./fuzzy-finding.md).

This function supports a range of named parameters that adjust its functionality:

- `:full` (boolean): If true, occupy the entire screen.
- `:prompt` (string): The text that will be shown beneath the search window.
- `:reverse` (boolean): Display from the top of the screen (rather than the bottom.)
- `:animated` (boolean): Enable and disable background animation.
