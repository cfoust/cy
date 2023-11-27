import json
import sys
import re
import hashlib

STORY_REGEX = re.compile("{{((\w+).)?(png|gif) (.+)}}")

if __name__ == '__main__':
    args = sys.argv
    if len(args) > 1 and args[1] == "supports":
        sys.exit(0)

    context, book = json.load(sys.stdin)

    # all the rendering jobs that need to be done
    jobs = {}

    for section in book['sections']:
        if not 'Chapter' in section:
            continue

        content = section['Chapter']['content']
        replace = []

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

            jobs[filename] = command
            replace.append(
                (
                    ref.start(0),
                    ref.end(0),
                    f"![{command}](./stories/{filename})",
                )
            )

        for start, end, text in reversed(replace):
            content = content[:start] + text + content[end:]

        section['Chapter']['content'] = content

    print(json.dumps(book))
