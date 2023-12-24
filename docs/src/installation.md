# Installation

You can install `cy` using pre-compiled binaries or from source.

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
