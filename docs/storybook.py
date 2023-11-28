import hashlib
import json
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

STORY_REGEX = re.compile("{{((\w+).)?(png|gif) (.+)}}")

if __name__ == '__main__':
    args = sys.argv
    if len(args) > 1 and args[1] == "supports":
        sys.exit(0)

    context, book = json.load(sys.stdin)

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

            filename += "." + type_
            filename = "images/" + filename

            replace.append(
                (
                    ref.start(0),
                    ref.end(0),
                    f"![{command}]({filename})",
                )
            )

            filename = "./src/" + filename
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

    for filename, command in jobs.items():
        if os.path.exists(filename): continue

        print(f"~> building {filename} ({command})", file=sys.stderr)

        script = ""
        if filename.endswith(".gif"):
            script = f"""
Output {filename}
Set Padding 0
Set Framerate 23
Set PlaybackSpeed 0.5
Hide
Type "./stories -s {command} && clear"
Enter
Sleep 500ms
Show
Sleep 8s
"""
        elif filename.endswith(".png"):
            script = f"""
Set Padding 0
Hide
Type "./stories -s {command} && clear"
Enter
Sleep 2s
Show
Sleep 1s
Screenshot {filename}
"""

        tape = (
            filename.replace("png", "tape")
            .replace("gif", "tape")
        )
        with open(tape, 'w') as f:
            f.write(script)

        vhs = "vhs"
        if 'CI' in os.environ:
            vhs = "./vhs"

        code = subprocess.call(
            f"{vhs} -q {tape}",
            shell=True
        )

        if code != 0:
            raise Exception(code)

    if os.path.exists(tape): os.unlink(tape)

    print(json.dumps(book))
