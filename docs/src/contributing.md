# Contributing

`cy` is still a young project and consequently does not have a rigorous contribution process nor a comprehensive list of upcoming features. Contributions of any kind are welcome.

Most new code should have tests. It should also pass `cy`'s continuous integration (CI), which is described [in more detail below](/contributing.md#passing-ci). For more specific information of interest to contributors, you can peruse the other pages that follow this one in the **Developer guide** section.

## Common commands

To hack on `cy`, you only need a recent [Go toolchain](https://go.dev/doc/install) and a clone of [the cy repository](https://github.com/cfoust/cy).

For convenience, `cy` provides a configuration file for [just](https://github.com/casey/just), so you can execute commands like `just build`, `just lint`, `just test`, and `just run` and they will work as expected.

You can build and run `cy` from source with the following command. This is convenient for testing small changes.

```bash
go run ./cmd/cy/...
```

Note that this will connect to the default `cy` [socket](/cli.md#the---socket-name-flag). If you already use `cy` to write code, you may want to run `cy` with `cy -L dev` to ensure the server you're using to code is unaffected by the version you have checked out.

You can also build or install cy from source:

```bash
go build ./cmd/cy/...
go install ./cmd/cy/...
```

Most code in `cy` has tests. You can run all of `cy`'s tests like this:

```bash
go test ./cmd/... ./pkg/...
```

Formatting code works similarly:

```bash
go fmt ./cmd/... ./pkg/...
```

## The cy development workflow

There are a few different workflows I use when contributing to `cy`:

1. **Run `cy` directly (e.g. with `go run`).** This works best for small changes or UI states that are easy to access.
2. **Write tests.** A good rule of thumb is that anything resembling a state machine should have tests. Any changes or additions to the Janet API also should have comprehensive tests.
3. **Write [stories](/stories.md).** All UI changes can be represented as `cy` stories, which are small, preconfigured scenarios that you can browse easily with the story interface.

### Janet tests

The `cy` codebase has a special mechanism for writing tests for the Janet API. You can run these tests with the following command:

```bash
go test ./pkg/cy/ -run TestAPI
```

To write a new test, add a new Janet file in the `pkg/cy/api` directory matching the `*_test.janet` pattern.

Janet tests look like this:

```janet
# ignore
(test "name of the test"
      # do some stuff
      (assert (= true true)))
```

The `test` macro runs the code that it contains in an isolated, in-memory `cy` environment.

## Passing CI

CI is simple. Your code need only:

1. Pass tests
2. Be properly formatted

## Pull requests

There is currently no standard format for pull requests. I (cfoust) will review your code, make suggestions for improvements if necessary, or merge it if there is nothing left to address.

`cy` has a loosely-obeyed release cycle, so depending on the contribution I may either merge it straight to `main` or put it into the next release branch. Bug fixes always go out as soon as possible.
