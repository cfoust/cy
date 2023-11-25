# Fuzzy finding

Fuzzy finding, popularized by [fzf](https://github.com/junegunn/fzf), has taken the software development world by storm. To that end, `cy` provides a purpose-built fuzzy finder that imports and improves upon a range of functionality from `fzf` in the form of `(input/find)`, which is a function available in `cy`'s API.

This function is complicated enough and provides significant enough functionality that it deserves its own independent chapter.

## Choosing a string from a list

In its simplest form, `(input/find)` takes a single parameter: a [Janet array](https://janet-lang.org/docs/data_structures/arrays.html) of strings that it will present to the user and from which they can choose a single option:

```janet
(input/find @["one" "two" "three"])
```

By default, the background will be animated with one of `cy`'s [animations](./animations.md). If the user does not choose anything, this function returns `nil`; if they do, it will return the option that they chose.

## Choosing an arbitrary value from a list

`(input/find)` also allows you to ask the user to choose from a list of items, each of which has an underlying Janet value that is returned instead of the string value that the user filters. You do this by providing a Janet array of tuples, each of which has two elements:

- The text to be filtered
- The value that should be returned

```janet
(input/find @[
    ["one" 1]
    ["two" 2]
    ["three" 3]])
```

If the user chooses `"one"`, `(input/find)` will return `1`.

## Previews

Where `(input/find)` really shines, however, is in its ability to show a preview window for each option, which is conceptually similar to `fzf`'s `--preview` command line flag. `(input/find)` can preview three different types of content:

- **Panes:** Show the current state of a pane in `cy`'s [node tree](./groups-and-panes.md#the-node-tree). This is the live view of a pane, regardless of how many other clients are interacting with it or what is happening on the screen.
- **`.borg` files:** Show a moment in time in a `.borg` file.
- **Text** Render some text (with, eventually, rich formatting).
