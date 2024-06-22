"""
This is a preprocessor for mdbook that generates Markdown programmatically,
such as for API and key binding documentation.
"""
import json
import os
import re
import subprocess
import sys
from pathlib import Path

GENDOC_REGEX = re.compile("{{gendoc (.+)}}")

if __name__ == '__main__':
    args = sys.argv
    if len(args) > 1 and args[1] == "supports":
        sys.exit(0)

    context, book = json.load(sys.stdin)

    if subprocess.call(
        "go build -o gendoc ../cmd/docs/main.go",
        shell=True
    ) != 0:
        raise Exception("failed to build gendoc")

    data = subprocess.run(
        "go run ../cmd/docs/main.go",
        shell=True,
        capture_output=True,
    )
    data.check_returncode()

    api = json.loads(data.stdout.decode('utf-8'))

    def transform_chapter(chapter):
        replace = []

        content = chapter['content']
        for ref in GENDOC_REGEX.finditer(content):
            command = ref.group(1)
            if len(command) == 0:
                continue

            output = ""

            if command == "frames":
                output = "\n---\n"
                for frame in api['Frames']:
                    output += f"""
#### {frame}

```janet
(viewport/set-frame "{frame}")
```

{{{{story png frame/{frame}}}}}

---
"""
            elif command == "animations":
                output = "\n---\n"
                for animation in api['Animations']:
                    output += f"""
#### {animation}

{{{{story gif animation/{animation}}}}}

---
                    """
            elif command == "api":
                output += "## Symbols\n\n"

                # Generate the table of contents
                for symbol in api['Symbols']:
                    name = symbol['Name']
                    link = (
                        name
                        .replace("?", "")
                        .replace("/", "")
                    )
                    output += f"[{name}](#{link}) "

                output += "\n\n---\n"

                for symbol in api['Symbols']:
                    _type = "function"

                    if symbol['Macro']:
                        _type = "macro"

                    source = ""
                    if symbol['Link']:
                        source = f"[source]({symbol['Link']})"

                    lines = symbol['Docstring'].split("\n")
                    if not lines:
                        continue

                    rest = ""

                    if len(lines) > 1:
                        rest = "\n" + "\n".join(lines[1:])

                    output += f"""
### {symbol['Name']}

{_type}

```janet
{lines[0]}
```{rest}

{source}

"""

            replace.append(
                (
                    ref.start(0),
                    ref.end(0),
                    output,
                )
            )

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

    print(json.dumps(book))
