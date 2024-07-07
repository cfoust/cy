# taro

Package taro is a high level framework for defining terminal interfaces that obey cy's `Screen` interface. It is a fork of [charmbracelet/bubbletea](https://github.com/charmbracelet/bubbletea) and borrows that library's state machine paradigm, originally inspired by [the Elm framework](https://elm-lang.org/).

I wanted bubbletea `Program`s to be able to write to arbitrary parts of the screen without futzing with strings. I also needed to improve on bubbletea's key/mouse event parsing (which, at any rate, has since been patched).
