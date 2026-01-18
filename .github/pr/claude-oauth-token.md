# claude: add oauth token support for sprite boxes

When running inside a sprite box (detected by `/.sprite` existing), read `~/.config/box/env.lua` and set `CLAUDE_CODE_OAUTH_TOKEN` from `claude.token` before exec.

## Changes

- `lib/claude/main.tl` - add `load_box_env()` to read box config, set oauth token in environment when inside sprite
