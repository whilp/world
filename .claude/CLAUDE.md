## Skills

- ALWAYS consider available skills. Almost every session will involve one or more skills, and the most relevant skills should be evident early in the session.

## Writing

- Always use sentence case
- Remove verbose comments unless they're very useful
- Prefer minimal, self-documenting code

## SQL

- always write sql in lowercase style for trino

## Searching

- ALWAYS use `rg` (ripgrep) to explore files
- NEVER use `find`
- NEVER use `grep`
- to find files with `rg`: `rg --files -g '*.<extension>' <path>`; if this doesn't work, consider whether it is appropriate to ignore gitignore and then try again with `-uuu`

## Python

- ALWAYS invoke `python3`
- NEVER invoke plain `python`

## Docs

- ALWAYS fetch the .md version of docs; for <https://docs.anthropic.com/en/docs/claude-code/hooks> -> <https://docs.anthropic.com/en/docs/claude-code/hooks.md>

## Gists

- to create a gist, do `gh create <file> --desc "<description>"`

## Git

- always use atomic commits
- write commit messages like '<component>: <action>'; eg 'comrak-fmt: rewrite in lua'
- git is usually configured with `status.showUntrackedFiles`; check `.gitconfig` when in doubt
- never invoke git -A; always add specific files

## Lua

See the lua skill for detailed patterns and templates

### Command execution
- NEVER use `os.execute()` - use spawn module or unix API instead
- NEVER use `io.popen()` - use spawn module instead
- example: `local spawn = require("spawn").spawn; spawn({"cmd", "arg1", "arg2"}):wait()`

### File system operations
- use `unix.*` functions: `unix.makedirs()`, `unix.rmrf()`, `unix.stat()`, `unix.opendir()`
- NEVER use string concatenation for paths - use `cosmo.path.join()` instead
- file permissions: use `tonumber("644", 8)` not `0644` (lua has no octal notation)

### Other patterns
- NEVER manipulate `package.path`
- avoid magic numbers in comparisons - use named constants or predicates
- use `cosmo.Fetch` instead of curl for http requests
- use transaction pattern with `pcall` for error handling

### Code organization
- library modules go in `lib/<name>/init.lua` or `lib/<name>/*.lua`
- binaries in `.local/bin` are simple wrappers that require the lib module
- tests go in `test_*.lua` files
