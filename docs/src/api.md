# API

This is a complete listing of all of the API functions that `cy` provides in the top-level Janet environment (ie you do not need to `import` or `use` anything.)

Janet code executed with `cy` can also access everything from [Janet's standard library](https://janet-lang.org/api/index.html) except for anything in `spork`. In addition, Janet's `ev` family of functions probably will not work; I have never tested them.

## Concepts

#### NodeID

Many API functions have a parameter of type `NodeID`, which can be one of two values:

- `:root` which is a short way of referring to `cy`'s top-level group.
- An integer that refers to a node in `cy`'s [node tree](groups-and-panes.md#the-node-tree). You cannot infer these yourself, but they are returned from API functions like [`(pane/current)`](api.md#panecurrent) and [`(group/children)`](api.md#groupchildren).

{{gendoc api}}
