#!/bin/bash
# PostToolUse hook: format and lint Go files after Edit/Write
INPUT=$(cat)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')

# Only process .go files
if [[ "$FILE_PATH" != *.go ]]; then
  exit 0
fi

# Check the file still exists (Write could have been to a new file that was then removed, etc.)
if [[ ! -f "$FILE_PATH" ]]; then
  exit 0
fi

# Format the file
gofmt -w "$FILE_PATH" 2>&1

# Lint the file with auto-fix
cd "$CLAUDE_PROJECT_DIR"
go tool golangci-lint run --fix "$FILE_PATH" 2>&1

exit 0
