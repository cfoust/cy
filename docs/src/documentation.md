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

The `story` tag allows you to render [stories](/stories.md) as static PNGs, animated GIFs, or an interactive [asciinema](https://docs.asciinema.org/) player.

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

### Janet code

All multiline Janet code blocks (e.g. those that begin with \`\`\`janet) are compiled and executed in a `cy` testing environment. An example that fails to compile or triggers an error while executing will cause documentation generation to fail. In other words, CI will not pass.

### Ignoring code blocks

You can tell the preprocessor to ignore a code block by putting `# ignore` in the first line. This line will be stripped.

```bash
# ignore
```

### Executing, but hiding parts of code blocks

You may also include some lines when the code block is being executed, but exclude them from what's displayed on the documentation site. This is useful for running any necessary setup.

You do this using `# {` (to start hiding lines) and `# }` (to stop hiding them).

Here's an example taken from the keybindings documentation:

```lisp
# {
(defn do-something [] )
(defn do-something-else [] )
# }
(key/bind-many :root
               ["ctrl+b" "1"] do-something
               ["ctrl+b" "2"] do-something-else)
```

The resulting code block would only have the following contents on the actual site:

```lisp
(key/bind-many :root
               ["ctrl+b" "1"] do-something
               ["ctrl+b" "2"] do-something-else)
```
