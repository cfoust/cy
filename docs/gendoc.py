"""
This is a preprocessor for mdbook that generates Markdown programmatically,
such as for API and key binding documentation.
"""
import json
import os
import re
import subprocess
import argparse
import sys
from pathlib import Path
from typing import (
    NamedTuple,
    Optional,
    Tuple,
    Dict,
    List,
    Any,
    Set,
    Callable,
)


class Symbol(NamedTuple):
    Name: str
    Docstring: str
    Link: str
    Macro: bool


class Param(NamedTuple):
    Name: str
    Docstring: str
    Default: str
    Type: str


class Binding(NamedTuple):
    Tag: str
    Source: str
    Function: Optional[Symbol]
    Sequence: List[str]


def render_frames(frames: List[str]) -> str:
    output = "\n---\n"
    for frame in frames:
        output += f"""
#### {frame}

```janet
# ignore
(viewport/set-frame "{frame}")
```

{{{{story png frame/{frame}}}}}

---
"""
    return output


def symbol_to_url(symbol: Symbol) -> str:
    url = (
        symbol.Name
        .replace("?", "")
        .replace("/", "")
    )
    return f"./api.md#{url}"


def render_symbol_link(symbol: Symbol) -> str:
    return f"[{symbol.Name}]({symbol_to_url(symbol)})"


def render_animations(animations: List[str]) -> str:
    output = "\n---\n"
    for animation in api['Animations']:
        output += f"""
#### {animation}

{{{{story gif animation/{animation}}}}}

---
        """
    return output


def render_api(symbols: List[Symbol]) -> str:
    output = "## Symbols\n\n"

    # Generate the table of contents
    for symbol in symbols:
        output += render_symbol_link(symbol) + " "

    output += "\n\n---\n"

    for symbol in symbols:
        _type = "function"

        if symbol.Macro:
            _type = "macro"

        source = ""
        if symbol.Link:
            source = f"[source]({symbol.Link})"

        lines = symbol.Docstring.split("\n")
        if not lines:
            continue

        rest = ""

        if len(lines) > 1:
            rest = "\n" + "\n".join(lines[1:])

        output += f"""
### {symbol.Name}

{_type}

```janet
# ignore
{lines[0]}
```{rest}

{source}

"""
    return output


def render_params(params: List[Param]) -> str:
    output = ""

    # Generate the table of contents
    for param in params:
        output += f"[{param.Name}](./parameters.md#{param.Name}) "

    output += "\n\n---\n"

    for param in params:
        output += f"""
### {param.Name}

**Type:** `:{param.Type}`

**Default:** `{param.Default}`

{param.Docstring}

"""
    return output


def render_key(key: str) -> str:
    if key == " ":
        return "space"
    return key


def render_key_sequence(sequence: List[str]) -> str:
    return " ".join(map(
        lambda a: f"<kbd>{render_key(a)}</kbd>",
        sequence,
    ))


def render_keys(bindings: List[Binding], args: List[str]) -> str:
    parser = argparse.ArgumentParser()
    parser.add_argument('source')
    parser.add_argument('group')
    parser.add_argument('--skip', type=int, default=0)
    params = parser.parse_args(args)

    output = """
| Sequence    | Action  | Description |
| ----------- | ------- | ----------- |
    """

    bindings = list(filter(
        lambda a: a.Source == params.source and a.Tag == params.group,
        bindings,
    ))

    bindings = sorted(
        bindings,
        key=lambda a: a.Function.Name if a.Function else "",
    )

    for bind in bindings:
        symbol = bind.Function
        if not symbol: continue
        link = render_symbol_link(symbol)
        sequence = render_key_sequence(bind.Sequence[params.skip:])
        description = symbol.Docstring.split("\n")[2]
        output += f"""| {sequence} | {link} | {description} |\n"""

    return output

Error = Tuple[int, str]
Transformer = Callable[[str],Tuple[str, List[Error]]]
Replacement = Tuple[int, int, str]


def handle_pattern(
    pattern: re.Pattern,
    handler: Callable[
        [re.Match],
        Tuple[Optional[Replacement], Optional[Error]],
    ],
) -> Transformer:
    """
    Given a regex pattern `pattern` and a function `handler` that turns matches
    into in-text replacements, return a Transformer.
    """

    def transform(content: str) -> Tuple[str, List[Error]]:
        replace: List[Replacement] = []
        errors: List[Error] = []

        for match in pattern.finditer(content):
            replacement, error = handler(match)
            if not replacement:
                if error: errors.append(error)
                continue

            replace.append(replacement)

        replace = sorted(replace, key=lambda a: a[1])

        for start, end, text in reversed(replace):
            content = content[:start] + text + content[end:]

        return content, errors

    return transform


def transform_gendoc(
    frames: List[str],
    animations: List[str],
    symbols: List[Symbol],
    params: List[Param],
) -> Transformer:
    def handler(match: re.Match) -> Tuple[
            Optional[Replacement],
            Optional[Error],
    ]:
        command = match.group(1)
        if len(command) == 0:
            return None, None

        output = ""
        if command == "frames":
            output = render_frames(frames)
        elif command == "animations":
            output = render_animations(animations)
        elif command == "api":
            output = render_api(symbols)
        elif command == "params":
            output = render_params(params)

        return (
            match.start(0),
            match.end(0),
            output,
        ), None

    return handle_pattern(re.compile("{{gendoc (.+)}}"), handler)


def transform_keys(
    bindings: List[Binding],
) -> Transformer:
    def handler(match: re.Match) -> Tuple[
            Optional[Replacement],
            Optional[Error],
    ]:
        args = match.group(1)
        if len(args) == 0:
            return None, None

        return (
            match.start(0),
            match.end(0),
            render_keys(
                bindings,
                args.split(" "),
            ),
        ), None

    return handle_pattern(re.compile(r"{{keys (.+)}}"), handler)


def transform_api(
    symbol_lookup: Dict[str, Symbol],
) -> Transformer:
    def handler(match: re.Match) -> Tuple[
            Optional[Replacement],
            Optional[Error],
    ]:
        name = match.group(1)
        if len(name) == 0:
            return None, None

        if not name in symbol_lookup:
            return None, (
                match.start(0),
                f"missing symbol: {name}",
            )

        symbol = symbol_lookup[name]

        return (
            match.start(0),
            match.end(0),
            render_symbol_link(symbol),
        ), None

    return handle_pattern(re.compile(r"{{api ([a-z0-9/-]+)}}"), handler)


def transform_bind(
    bindings: List[Binding],
) -> Transformer:
    """
    This Transformer turns key references into <kbd> elements with a link to
    the action that will be executed when the key sequence is typed.
    """
    def handler(match: re.Match) -> Tuple[
            Optional[Replacement],
            Optional[Error],
    ]:
        source = match.group(1)
        sequence = match.group(2).split(' ')

        binding: Optional[Binding] = None
        for bind in bindings:
            if (
                bind.Sequence == sequence and
                bind.Source == source
            ):
                binding = bind
                break

        if not binding:
            return None, (
                match.start(0),
                f"missing binding: :{source} {' '.join(sequence)}",
            )

        symbol = binding.Function
        if not symbol:
            return None, (
                match.start(0),
                f"binding is missing symbol: :{source} {' '.join(sequence)}",
            )

        key_sequence = render_key_sequence(binding.Sequence)
        return (
            match.start(0),
            match.end(0),
            f"{key_sequence} [[?]]({symbol_to_url(symbol)})",
        ), None

    return handle_pattern(
        re.compile(r"{{bind :(\w+) (([0-9a-z+-]+\s*)+)}}"),
        handler,
    )


def transform_packages() -> Transformer:
    pkg_dir = os.path.join(os.path.dirname(__file__), "..", "pkg")

    packages: List[Tuple[str, str]] = []

    for dir, _, _ in os.walk(pkg_dir):
        relative = os.path.relpath(dir, start=pkg_dir)
        readme = os.path.join(dir, "README.md")
        if not os.path.exists(readme): continue

        with open(readme, 'r') as f:
            packages.append((
                relative,
                f.read(),
            ))

    packages = sorted(
        packages,
        key=lambda a: a[0],
    )

    docs = ""

    for name, readme in packages:
        lines = readme.split("\n")
        # Skip the first line, usually #
        lines = lines[1:]
        # Increase header level
        lines = list(map(
            lambda line: "#" + line if line.startswith("#") else line,
            lines,
        ))
        readme = "\n".join(lines)
        docs += f"""## {name}

[source](https://github.com/cfoust/cy/tree/main/pkg/{name})

{readme}"""

    def handler(match: re.Match) -> Tuple[
            Optional[Replacement],
            Optional[Error],
    ]:
        return (match.start(0), match.end(0), docs,), None

    return handle_pattern(re.compile(r"{{packages}}"), handler)


IGNORE_CODE = "# ignore"
HIDDEN_CODE_START = "# {"
HIDDEN_CODE_END = "# }"


def transform_examples(runner: subprocess.Popen) -> Transformer:
    """
    Ensure all example Janet code runs correctly.
    """
    def handler(match: re.Match) -> Tuple[
            Optional[Replacement],
            Optional[Error],
    ]:
        lines = match.group(1).strip().split("\n")

        # Support directives to hide code from the reader
        filtered: List[str] = []
        hiding: bool = False
        ignored: bool = False
        for line in lines:
            if hiding: continue

            if line == IGNORE_CODE:
                ignored = True
                continue

            if (
                line != HIDDEN_CODE_START and
                line != HIDDEN_CODE_END
            ):
                filtered.append(line)
                continue

            hidden = line == HIDDEN_CODE_START

        code = '\n'.join(filtered)
        code = f"```janet\n{code}\n```"

        if ignored:
            return (
                match.start(0),
                match.end(0),
                code,
            ), None

        example_code = '\n'.join(lines)

        if not runner.stdout or not runner.stdin:
            return None, (
                match.start(0),
                "runner not initialized",
            )

        runner.stdin.write(example_code)
        runner.stdin.flush()

        output_lines = []
        while True:
            output = runner.stdout.readline().strip()
            if output == 'ok': break
            if output == 'error': break
            output_lines.append(output)

        if output_lines:
            error_msg = '\n'.join(output_lines)
            return None, (
                match.start(0),
                f"failed executing Janet code: {error_msg}",
            )

        return (
            match.start(0),
            match.end(0),
            code,
        ), None

    return handle_pattern(
        re.compile(r"```janet([^`]+)```", re.MULTILINE),
        handler,
    )


if __name__ == '__main__':
    args = sys.argv
    if len(args) > 1 and args[1] == "supports":
        sys.exit(0)

    context, book = json.load(sys.stdin)

    data = subprocess.run(
        "go run ../cmd/docs/main.go",
        shell=True,
        capture_output=True,
    )
    data.check_returncode()

    api = json.loads(data.stdout.decode('utf-8'))

    symbols: List[Symbol] = []
    for symbol in api['Symbols']:
        symbols.append(Symbol(**symbol))

    params: List[Param] = []
    for param in api['Parameters']:
        params.append(Param(**param))

    symbol_lookup = {x.Name: x for x in symbols}

    bindings: List[Binding] = []
    for binding in api['Binds']:
        func = binding['Function']
        if not func in symbol_lookup: continue
        binding['Function'] = symbol_lookup[func]
        bindings.append(Binding(**binding))

    runner = subprocess.Popen(
        ["go", "run", "../cmd/example/main.go"],
        text=True,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=sys.stderr,
        encoding="utf-8",
    )

    if runner.stdout:
        runner.stdout.readline()

    transformers: List[Transformer] = [
        transform_packages(),
        transform_keys(bindings),
        transform_gendoc(
            api['Frames'],
            api['Animations'],
            symbols,
            params,
        ),
        transform_api(symbol_lookup),
        transform_bind(bindings),
        transform_examples(runner),
    ]

    num_errors: int = 0

    def transform_chapter(chapter) -> None:
        global num_errors
        content: str = chapter['content']

        for transform in transformers:
            content, errors = transform(content)

            for index, message in errors:
                num_errors += 1
                # not accurate since other transformers may have changed this,
                # but whatever
                line = len(content[:index].split("\n"))

                print(
                    f"{chapter['name']}:{line}: {message}",
                    file=sys.stderr,
                )

        chapter['content'] = content

        for subitem in chapter['sub_items']:
            if not 'Chapter' in subitem:
                continue

            transform_chapter(subitem['Chapter'])

    for section in book['sections']:
        if not 'Chapter' in section:
            continue

        transform_chapter(section['Chapter'])

    if num_errors > 0:
        print(f"{num_errors} error(s) while preprocessing", file=sys.stderr)
        exit(1)

    runner.kill()
    print(json.dumps(book))
