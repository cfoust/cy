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

    def transform_chapter(chapter):
        replace = []

        content = chapter['content']
        for ref in GENDOC_REGEX.finditer(content):
            command = ref.group(1)
            if len(command) == 0:
                continue

            result = subprocess.run(
                f"./gendoc {command}",
                shell=True,
                capture_output=True,
            )
            result.check_returncode()

            replace.append(
                (
                    ref.start(0),
                    ref.end(0),
                    result.stdout.decode('utf-8'),
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
