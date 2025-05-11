# Installation

You can install `cy` using pre-compiled binaries or from source.

**Note:** `cy` is still highly experimental. Some things may not work very well, but I would appreciate it if you [created an issue](https://github.com/cfoust/cy/issues) if something breaks or you spot something `cy` could do better.

### brew

You can use `brew` to install `cy` on macOS and Linux:

```bash
# Install the latest version:
brew install cfoust/taps/cy

# Or a specific one:
brew install cfoust/taps/cy@0.1.8
```

### From binary

Download the latest version of `cy` on [the releases page](https://github.com/cfoust/cy/releases) and extract the archive that corresponds to your operating system and architecture into your `$PATH`.

### From source

```bash
git clone git@github.com:cfoust/cy.git
cd cy
go install ./cmd/cy/.
```

## Terminal emulators

It is recommended that you use [kitty](https://sw.kovidgoyal.net/kitty/) to run `cy`. However, `cy` has also been tested extensively in [iTerm](https://iterm2.com/index.html); your mileage may vary.
