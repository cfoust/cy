---
name: release
description: Prepare a release by updating the changelog and creating a git tag
argument-hint: <version e.g. v1.10.2>
disable-model-invocation: true
allowed-tools: Bash(git *), Bash(gh *), Read, Edit, Grep, Glob
---

# Prepare a Release

You are preparing release `$ARGUMENTS` for the cy project.

## Steps

### 1. Validate the version argument

The version must match the format `vX.Y.Z` (e.g. `v1.10.2`). If it doesn't, stop and tell the user.

### 2. Find the last release tag

Run `git tag --sort=-v:refname | head -1` to get the most recent release tag.

### 3. Gather changes since the last release

Run `git log <last-tag>..HEAD --oneline` to get all commits since the last release. Read through them carefully to understand what changed.

### 4. Draft changelog entries

Based on the commit log, write concise changelog bullet points. Follow the existing style in `CHANGELOG.md`:

- Each entry is a single `- ` bullet point
- Focus on user-facing changes: features, fixes, improvements
- Skip purely internal changes (CI, refactoring, docs typos) unless significant
- Link to relevant docs or specs where appropriate (match existing style)
- Group related commits into a single entry when it makes sense

### 5. Show the user the draft

Present the drafted changelog entries to the user and ask for approval before making any changes. Wait for their confirmation.

### 6. Update CHANGELOG.md

Insert a new section at the top of the changelog (after the `# Changelog` heading) with the format:

```
## X.Y.Z - YYYY-MM-DD
```

Note: the heading uses the version **without** the `v` prefix. Use today's date.

### 7. Commit and tag

```bash
git add CHANGELOG.md
git commit -m "release: $ARGUMENTS"
git tag $ARGUMENTS
```

### 8. Remind the user

After tagging, remind the user to push the tag with:

```
git push origin main && git push origin $ARGUMENTS
```

Do NOT push automatically.
