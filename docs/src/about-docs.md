# About this site

This documentation is divided into three sections:

- **Quick start:** a basic tutorial on using `cy`.
- **User guide:** an introduction to `cy`'s concepts and functionality.
- **Reference:** a technical reference for `cy`'s API, its default key bindings, and much more.

### Documentation features

#### Key bindings

Key bindings are rendered like this: {{bind :root ctrl+a p}}.

The left portion is a sequence of keys that you can enter to trigger that key binding, which in this case is <kbd>ctrl+a</kbd> followed by <kbd>p</kbd>. In other words, hold control and hit the <kbd>a</kbd> key, release both keys, and then hit the <kbd>p</kbd> key.

The link on the right (`[?]`) points to the documentation for the Janet function that that key sequence executes.

#### Recordings

This documentation makes extensive use of [asciinema](https://docs.asciinema.org/manual/player/), a player for terminal sessions. It uses asciinema to demonstrate `cy`'s functionality and teach you how to use it. The white section on the left side of the window in the session rendered below shows a history of all of the keys entered so you can easily follow along on your own.

{{story cast cy/switch-shells}}

These are generated in CI and therefore should always be up to date.
