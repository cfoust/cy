# Stories

{{story cast stories --width 120 --height 26}}

> The above is the stories interface. Typing filters the list of stories and you can use <kbd>up</kbd> and <kbd>down</kbd> to move between them. Stories are not interactive, though this may change.

`cy`'s user interface is complex and some UI states are tedious to navigate to when you're trying to iterate quickly. To remedy this, the `cy` repository contains a mechanism (uncreatively) called **stories**.

A **story** is a preconfigured [`Screen`](/architecture.md#screen) along with an (optional) sequence of user inputs that will be played back on that screen. Every story also has a unique string name that looks like a path, e.g. `input/find/search`. After defining a story in Go code, you can open a special interface that lets you quickly view that story.

Stories can be registered by any package in `cy` and can be browsed in a central interface. 

This functionality was inspired by [Storybook](https://storybook.js.org/), a framework used to develop UI components.

## Viewing stories

Run the following to open the stories interface:

```bash
go run ./cmd/stories/...
```

Press <kbd>q</kbd> to quit at any time.

The stories executable accepts a range of arguments. Provide `--help` to see them all.

To run only a single story:

```bash
go run ./cmd/stories/... -s input/find/search
```

To filter the list of stories with a prefix:

```bash
go run ./cmd/stories/... -p input
```

Any stories with names that do not begin with `input` will be filtered out.

## Registering a new story

Stories are registered using the [`Register`](https://github.com/cfoust/cy/blob/main/pkg/stories/module.go?plain=1#L74) function in the [stories package](/packages.md#stories). Search the codebase for usage examples.
