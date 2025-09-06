---
allowed-tools: Bash(session-logger:*)
description: Export a Claude Code session using session-logger
---

# Gist the session

Run `session-logger` to export a Claude Code session to markdown format and share as a private GitHub gist.

## Usage

- The current transcript path should be obvious from context when the `session-logger --hook` `SessionStart` hook is enabled
- If the transcript path is not present, the user may need to add or fix their hook configuration
- `session-logger` takes a `--description` option that will be used as the description for the resulting gist

## Examples

``` bash
session-logger --gist --description "Setting up PostgreSQL" ~/.claude/projects/<project>/<session>.jsonl
```
