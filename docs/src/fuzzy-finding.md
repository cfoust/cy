# Fuzzy finding

{{story cast input/find/full-bottom}}

Simple, fast, and configurable fuzzy finding is one of `cy`'s most important features. `cy` provides a purpose-built fuzzy finder (similar to [fzf](https://github.com/junegunn/fzf)) in the form of [`(input/find)`](./api.md#inputfind), which is a function available in [the API](./api.md#inputfind).

## Choosing a string from a list

In its simplest form, [`(input/find)`](api.md#inputfind) takes a single parameter: a [Janet array](https://janet-lang.org/docs/data_structures/arrays.html) of strings that it will present to the user and from which they can choose a single option:

```janet
(input/find @["one" "two" "three"])
```

By default, the background will be animated with one of `cy`'s [animations](./animations.md). If the user does not choose anything, this function returns `nil`; if they do, it will return the option that they chose.

## Choosing an arbitrary value from a list

[`(input/find)`](api.md#inputfind) also allows you to ask the user to choose from a list of items, each of which has an underlying Janet value that is returned instead of the string value that the user filters.

You do this by providing a Janet array of tuples, each of which has two elements:

- The text to be filtered
- The value that should be returned

```janet
(input/find @[
    ["one" 1]
    ["two" 2]
    ["three" 3]])
```

If the user chooses `"one"`, [`(input/find)`](api.md#inputfind) will return `1`.

## Filtering tabular data

{{story cast input/find/table/full-bottom}}

It is sometimes handy to be able to have the user choose from a row in a table rather than a single line of text. [`(input/find)`](api.md#inputfind) allows you to provide tabular data in addition to titles for each column in the form of tuples.

```janet
(input/find
    @[
        [["boba" "$5" "not much"] 3]
        [["latte" "$7" "too much"] 2]
        [["black tea" "$2" "just right"] 1]
     ]
    # Providing headers is optional
    :headers ["drink" "price" "caffeine"])
```

## Choosing with previews

Where [`(input/find)`](api.md#inputfind) really shines, however, is in its ability to show a preview window for each option, which is conceptually similar to `fzf`'s `--preview` command line flag. [`(input/find)`](api.md#inputfind) can preview three different types of content:

- **Panes:** Show the current state of a pane in `cy`'s [node tree](./groups-and-panes.md#the-node-tree). This is the live view of a pane, regardless of how many other clients are interacting with it or what is happening on the screen.
- **`.borg` files:** Show a moment in time in a `.borg` file.
- **Text** Render some text.

Options with previews are passed to [`(input/find)`](api.md#inputfind) as Janet tuples with three elements:

1. The text (or columns) that the user will filter against
1. A Janet [table](https://janet-lang.org/docs/data_structures/tables.html) describing how this option should be previewed
1. The value that should be returned if the user chooses this option

Here are some examples:

```janet
(input/find @[
        # A standard text preview, which will be rendered in an 80x26 window
        ["some text" {:type :text :text "this is the preview"} 1]
        # A replay preview
        ["this is a borg file" {:type :replay :path "some-file.borg"} 2]
        # A pane preview
        ["this is some other pane" {:type :node :id (pane/current)} 3]
    ])
```

[`(input/find)`](api.md#inputfind) is used extensively in `cy`'s [default startup script](https://github.com/cfoust/cy/blob/main/pkg/cy/cy-boot.janet). You can find several idiomatic examples of its usage there.
