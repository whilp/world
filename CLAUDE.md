## Writing

- Always use sentence case

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

- ALWAYS fetch the .md version of docs; for <https://docs.anthropic.com/en/docs/claude-code/hooks> -\> <https://docs.anthropic.com/en/docs/claude-code/hooks.md>

## Gists

- to create a gist, do `gh create <file> --desc "<description>"`

## Shimlink

- symlink-based binary manager
- config: `~/.config/shimlink/shimlink.lua`
- binaries stored in versioned dirs: `~/.local/share/shimlink/_/<binary>/<sha>/`
- shimlink bin directory: `~/.local/share/shimlink/bin/` (symlinks to versioned binaries)
- update binary: `shimlink update <binary>`
- force update (skip checksum): `shimlink update -f <binary>`

### checksums

- shimlink checksums are calculated on the **extracted binary file**, not the archive
- for archives with `path: "bin/nvim"`, the sha256 should be of the extracted `bin/nvim` file
- to get the correct checksum: `curl -sL <url> | tar -xzf - && shasum -a 256 <path/to/binary>`
- example: `curl -sL https://github.com/.../nvim-macos-arm64.tar.gz | tar -xzf - && shasum -a 256 nvim-macos-arm64/bin/nvim`

## Git

- always use atomic commits
- write commit messages like '<component>: <action>'; eg 'comrak-fmt: rewrite in lua'
- git is usually configured with `status.showUntrackedFiles`; check `.gitconfig` when in doubt