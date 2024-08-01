# Fuzzy finding

{{story cast input/find/full-bottom}}

Simple, fast, and configurable fuzzy finding is one of `cy`'s most important features. `cy` provides a purpose-built fuzzy finder (similar to [fzf](https://github.com/junegunn/fzf)) in the form of {{api input/find}}, which is a function available in [the API](/api.md#inputfind).

## Choosing a string from a list

In its simplest form, {{api input/find}} takes a single parameter: a [Janet array](https://janet-lang.org/docs/data_structures/arrays.html) of strings that it will present to the user and from which they can choose a single option:

```janet
(input/find @["one" "two" "three"])
```

By default, the background will be animated with one of `cy`'s [animations](/animations.md). If the user does not choose anything, this function returns `nil`; if they do, it will return the option that they chose.

## Choosing an arbitrary value from a list

{{api input/find}} also allows you to ask the user to choose from a list of items, each of which has an underlying Janet value that is returned instead of the string value that the user filters.

You do this by providing a Janet array of tuples, each of which has two elements:

- The text to be filtered
- The value that should be returned

```janet
(input/find @[
    ["one" 1]
    ["two" 2]
    ["three" 3]])
```

If the user chooses `"one"`, {{api input/find}} will return `1`.

## Filtering tabular data

{{story cast input/find/table/full-bottom}}

It is sometimes handy to be able to have the user choose from a row in a table rather than a single line of text. {{api input/find}} allows you to provide tabular data in addition to titles for each column in the form of tuples.

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

Where {{api input/find}} really shines, however, is in its ability to show a preview window for each option, which is conceptually similar to `fzf`'s `--preview` command line flag. {{api input/find}} can preview three different types of content:

- **Panes:** Show the current state of a pane in `cy`'s [node tree](/groups-and-panes.md#the-node-tree). This is the live view of a pane, regardless of how many other clients are interacting with it or what is happening on the screen.
- **`.borg` files:** Show a moment in time in a `.borg` file.
- **Scrollback buffer:** Show the output of a particular command in a pane's scrollback buffer.
- **Text:** Render some text.
- **Animations:** Show one of `cy`'s [animations](/animations.md).
- **Frames:** Show one of `cy`'s [frames](/frames.md).

Options with previews are passed to {{api input/find}} as Janet tuples with three elements:

1. The text (or columns) that the user will filter against
1. A Janet [struct](https://janet-lang.org/docs/data_structures/structs.html) describing how this option should be previewed
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
        # A scrollback preview
        ["this is a moment in a pane's history" {
            :type :scrollback
            :focus [0 0]
            :highlights @[]
            :id (pane/current)} 2]
        # An animation preview
        ["this is an animation" {
            :type :animation
            :name "midjo"} 2]
        # A frame preview
        ["this is a frame" {:type :frame :name "puzzle"} 2]
    ])
```

{{api input/find}} is used extensively in `cy`'s [default startup script](https://github.com/cfoust/cy/blob/main/pkg/cy/boot/actions.janet). You can find several idiomatic examples of its usage there.
