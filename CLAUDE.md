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

- symlink-based binary manager; config at `~/.config/shimlink/shimlink.json`
- binaries stored in `~/.local/share/shimlink/<binary>/`
- create managed binary: `ln -s shimlink <binary>`
- force update: `shimlink -f <binary>`

## Services

### nvim service

#### systemd (Linux)

- user services are in `~/.config/systemd/user/`
- reload after changes: `systemctl --user daemon-reload`
- enable at login: `systemctl --user enable nvim`
- start now: `systemctl --user start nvim`
- check status: `systemctl --user status nvim`
- view logs: `journalctl --user -u nvim -f`

#### launchd (macOS)

- user services are in `~/Library/LaunchAgents/`
- load and start: `launchctl load ~/Library/LaunchAgents/com.user.nvim.plist && launchctl start com.user.nvim`
- load service: `launchctl load ~/Library/LaunchAgents/com.user.nvim.plist`
- unload service: `launchctl unload ~/Library/LaunchAgents/com.user.nvim.plist`
- start service: `launchctl start com.user.nvim` (must load first)
- stop service: `launchctl stop com.user.nvim`
- check status: `launchctl list | grep nvim`
- view logs: `log stream --predicate 'process == "nvim"'`
- always use atomic commits
- write commit messages like '<component>: <action>'; eg 'comrak-fmt: rewrite in lua'