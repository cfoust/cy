# Documentation site

`cy`'s documentation site lives in the repository's `docs` directory. It uses [mdbook](https://github.com/rust-lang/mdBook). After installing `mdbook`, you can serve the documentation site by running the following in the `docs` directory:

```bash
mdbook serve
```

## Preprocessors

`cy`'s documentation makes extensive use of [mdbook preprocessors](https://rust-lang.github.io/mdBook/format/configuration/preprocessors.html) to generate content and assets on the fly with the version of `cy` currently checked out in the repository.

A preprocessor allows you to define custom transformations of the site's raw Markdown. `cy` uses this for a range of things described below. Using preprocessors, the `cy` documentation site defines a suite of special markup tags used to generate documentation and assets from the `cy` code.

All of these markup tags are enclosed in double curly brackets (e.g. `{{some-tag}}`) to avoid interfering with Markdown directives. The documentation below omits these double brackets for the sake of implementation simplicity.

### Stories

The `story` tag allows you to render [stories](./stories.md) as static PNGs, animated GIFs, or an interactive [asciinema](https://docs.asciinema.org/) player.

The default filename for generated assets is the hash of the tag's arguments.

Some examples:

```bash
# Generate a gif of the splash story and insert it
story gif splash

# You can also specify a file name
story main.gif splash

# Insert a png snapshot of the splash story
story png splash

# Render an asciinema cast of the cy/replay story
story cast cy/replay

# You can also specify the terminal dimensions of the story, which will
# overwrite the dimensions in the story's configuration
story cast cy/viewport --width 120 --height 26
```

### API symbols

You can reference symbols in `cy`'s API using the `api` tag, which will link to that symbol's documentation in the API reference. This is also useful because if you reference a symbol that does not exist, the preprocessor reports the error and fails CI. This is an effort to prevent broken links after API changes.

For example:

```bash
api input/find
```
