"""
This is a preprocessor for mdbook that generates .png and .gif files for cy's
stories on demand using vhs.
"""

import hashlib
import json
import os
import re
import subprocess
import sys
from pathlib import Path
from multiprocessing import Pool, cpu_count
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

STORY_REGEX = re.compile(r"{{story ((\w+).)?(png|gif|cast) (.+)}}")

COMMON = """
Set FontSize 18
Set Width 1300
Set Height 650
Set Padding 0
"""

def build_file(job: Tuple[str, str]):
    filename, command = job
    if os.path.exists(filename): return

    print(f"~> building {filename} ({command})", file=sys.stderr)

    script = ""
    if filename.endswith(".gif"):
        script = f"""
{COMMON}
Output {filename}
Set Framerate 23
Set PlaybackSpeed 0.5
Hide
Type "./storybook -s {command} && clear"
Enter
Sleep 500ms
Show
Sleep 8s
"""
    elif filename.endswith(".png"):
        script = f"""
{COMMON}
Hide
Type "./storybook -s {command} && clear"
Enter
Sleep 2s
Show
Sleep 1s
Screenshot {filename}
"""
    elif filename.endswith(".cast"):
        subprocess.check_call(
            f"./storybook --cast {filename} -s {command}",
            shell=True,
            stdout=subprocess.DEVNULL,
            env={
                "TERM": "xterm-256color",
                "EDITOR": "/usr/bin/vim",
                "PS1": r" \[\e[0;31m\]▸▸▹\[\e[0m\] \[\e[0;31m\]\[\e[0m\]\[\033[00m\]",
            },
        )
        return

    tape = (
        filename.replace("png", "tape")
        .replace("gif", "tape")
    )
    with open(tape, 'w') as f:
        f.write(script)

    vhs = "vhs"
    if 'CI' in os.environ:
        vhs = "./vhs"

    while not os.path.exists(filename):
        subprocess.check_call(
            f"{vhs} -q {tape}",
            stdout=subprocess.DEVNULL,
            shell=True
        )

    os.unlink(tape)
    if not os.path.exists(filename):
        raise Exception(f"failed to produce {filename}")


if __name__ == '__main__':
    args = sys.argv
    if len(args) > 1 and args[1] == "supports":
        sys.exit(0)

    context, book = json.load(sys.stdin)

    # In CI, we use a custom storybook binary because we inject the latest cy
    # release version, so we don't need to build this again
    if not 'CI' in os.environ:
        if subprocess.call(
            "go build -o storybook ../cmd/stories/...",
            shell=True
        ) != 0:
            raise Exception("failed to build storybook")

    # all the rendering jobs that need to be done
    jobs = {}

    def transform_chapter(chapter):
        replace = []

        content = chapter['content']
        for ref in STORY_REGEX.finditer(content):
            type_ = ref.group(3)
            filename = ref.group(2)
            command = ref.group(4)
            if len(command) == 0:
                continue

            # The filename is the hash of the args, or can be specified in
            # Markdown
            if not filename:
                h = hashlib.new('sha256')
                h.update(command.encode('utf-8'))
                filename = h.hexdigest()[:12]

            original = filename

            filename += "." + type_

            filename = "/images/" + filename

            replacement = f"![{command}]({filename})"

            # Make image file relative to site
            if 'CI' in os.environ:
                replacement = f"![{command}](/cy/{filename[1:]})"

            if filename.endswith("cast"):
                replacement = f"<div data-cast=\"{original}\"></div>"

            replace.append(
                (
                    ref.start(0),
                    ref.end(0),
                    replacement,
                )
            )

            filename = "./src" + filename
            jobs[filename] = command

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

    Path("./src/images").mkdir(parents=True, exist_ok=True)

    if 'CY_SKIP_ASSETS' in os.environ:
        print(f"CY_SKIP_ASSETS enabled, not building assets", file=sys.stderr)
        jobs = {}

    with Pool(cpu_count()) as pool:
        list(pool.imap_unordered(
            build_file,
            jobs.items(),
        ))

    print(json.dumps(book))
