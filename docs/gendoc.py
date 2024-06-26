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
from typing import NamedTuple, Optional, Tuple, List, Any, Set

GENDOC_REGEX = re.compile("{{gendoc (.+)}}")
KEYS_REGEX = re.compile(r"{{keys (.+)}}")
API_REGEX = re.compile(r"{{api ([a-z0-9/-]+)}}")


class Symbol(NamedTuple):
    Name: str
    Docstring: str
    Link: str
    Macro: bool


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
(viewport/set-frame "{frame}")
```

{{{{story png frame/{frame}}}}}

---
"""
    return output


def render_symbol_link(symbol: Symbol) -> str:
    link = (
        symbol.Name
        .replace("?", "")
        .replace("/", "")
    )
    return f"[{symbol.Name}](./api.html#{link})"


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
{lines[0]}
```{rest}

{source}

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

    symbol_lookup = {x.Name: x for x in symbols}

    bindings: List[Binding] = []
    for binding in api['Binds']:
        func = binding['Function']
        if not func in symbol_lookup: continue
        binding['Function'] = symbol_lookup[func]
        bindings.append(Binding(**binding))

    errors: int = 0
    def report_error(chapter, start, end, msg):
        global errors
        errors += 1
        print(f"{chapter['name']}:{start}{end}: {msg}", file=sys.stderr)

    def transform_chapter(chapter) -> None:
        replace = []

        content = chapter['content']

        for ref in GENDOC_REGEX.finditer(content):
            command = ref.group(1)
            if len(command) == 0:
                continue

            output = ""
            if command == "frames":
                output = render_frames(api['Frames'])
            elif command == "animations":
                output = render_animations(api['Animations'])
            elif command == "api":
                output = render_api(symbols)

            replace.append(
                (
                    ref.start(0),
                    ref.end(0),
                    output,
                )
            )

        for ref in API_REGEX.finditer(content):
            name = ref.group(1)
            if len(name) == 0:
                continue

            if not name in symbol_lookup:
                report_error(
                    chapter,
                    ref.start(0),
                    ref.end(0),
                    f"missing symbol: {name}",
                )
                continue

            symbol = symbol_lookup[name]

            replace.append(
                (
                    ref.start(0),
                    ref.end(0),
                    render_symbol_link(symbol),
                )
            )

        for ref in KEYS_REGEX.finditer(content):
            args = ref.group(1)
            if len(args) == 0:
                continue

            replace.append(
                (
                    ref.start(0),
                    ref.end(0),
                    render_keys(
                        bindings,
                        args.split(" "),
                    ),
                )
            )

        replace = sorted(replace, key=lambda a: a[1])
        for start, end, text in reversed(replace):
            content = content[:start] + text + content[end:]

        chapter['content'] = content

        for subitem in chapter['sub_items']:
            if not 'Chapter' in subitem:
                continue

            transform_chapter(subitem['Chapter'])

    for section in book['sections']:
        if not 'Chapter' in section:
            continue

        transform_chapter(section['Chapter'])

    if errors > 0:
        print(f"{errors} error(s) while preprocessing")
        exit(1)

    print(json.dumps(book))
