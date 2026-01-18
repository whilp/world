# nvim: ruthlessly simplify wrapper

Simplified from 500 to 395 lines (21% reduction) by removing unnecessary complexity and using nvim's native `--server` flag instead of custom `--socket`.

## Changes

**Removed complexity (~105 lines):**
- Unused getopt import
- Signal handlers (never executed in client/daemon modes)
- Zsh environment loading (daemon now inherits current env)
- VIMRUNTIME auto-detection (nvim auto-detects it)
- Socket connection validation (simplified to file existence check)
- Unused daemon commands (restart, cleanup)
- `parse_socket_option()` function

**Changed approach:**
- Use nvim's native `--server` flag instead of custom `--socket` flag
- Extract socket from `--server` args instead of parsing/stripping custom flag
- Pass args through to nvim unchanged (no rebuilding)

**More robust:**
- Simpler code = fewer edge cases
- Less environment manipulation = more predictable
- File existence check handles permissions/stale sockets better
- Uses standard nvim flags

**All functionality verified:**
- `nvim --version` ✓
- `nvim --remote-ui` ✓
- `nvim --server /custom/path.sock --remote-ui` ✓
- `NVIM_SOCKET=/path nvimd start` ✓
- Daemon auto-start ✓

## Files

- `lib/nvim/main.tl` - ruthless simplification (500→395 lines)
- `.claude/skills/nvim/SKILL.md` - update docs to use `--server` instead of `--socket`
