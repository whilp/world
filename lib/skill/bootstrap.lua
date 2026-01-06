-- bootstrap - setup environment for Claude Code web
-- teal ignore: type annotations needed

local unix = require("cosmo.unix")

local CONTEXT = [[
# cosmic: cosmopolitan lua runtime

Portable lua with HTTP, JSON, crypto, compression, and POSIX APIs.

## Key modules

- **cosmo** - Fetch (HTTP), EncodeJson/DecodeJson, crypto, compression
- **cosmo.path** - join, basename, dirname, exists, isdir, isfile
- **cosmo.unix** - POSIX syscalls and file operations
- **cosmic.spawn** - `spawn({"cmd", "arg"}):wait()` (NEVER use os.execute/io.popen)

## Skills

`cosmic --skill pr` - manage PRs from .github/pr/<name>.md files. Add `x-cosmic-pr-name: <file>.md` trailer to commits.

## Updating cosmic

To adopt cosmic in a project:

1. **bin/cosmic** - wrapper script that downloads cosmic-lua from release URL:
   ```sh
   RELEASE_URL="https://github.com/whilp/world/releases/download/<tag>/cosmic-lua"
   ```
   Update the URL, delete old `bin/cosmic-lua` to trigger re-download.

2. **.claude/settings.json** - SessionStart hook to provide context:
   ```json
   {"hooks":{"SessionStart":[{"matcher":"startup","hooks":[
     {"type":"command","command":"\"$CLAUDE_PROJECT_DIR/bin/cosmic\" --skill bootstrap"}
   ]}]}}
   ```

## Constraints

- Use `path.join()` for paths (not string concatenation)
- Use `tonumber("644", 8)` for permissions (no octal in lua)
]]

local function main()
  print(CONTEXT)

  local env_file = os.getenv("CLAUDE_ENV_FILE")
  if not env_file then
    return 0
  end

  local cwd = unix.getcwd()
  if not cwd then
    return 1, "bootstrap: failed to get current working directory"
  end

  local bin_path = cwd .. "/bin"
  local export_line = string.format('export PATH="%s:$PATH"\n', bin_path)

  local f, err = io.open(env_file, "a")
  if not f then
    return 1, string.format("bootstrap: failed to open %s: %s", env_file, err)
  end

  local ok, write_err = f:write(export_line)
  f:close()

  if not ok then
    return 1, string.format("bootstrap: failed to write to %s: %s", env_file, write_err)
  end

  return 0
end

return {
  main = main,
  _VERSION = "0.2.0",
  _DESCRIPTION = "Bootstrap environment for Claude Code web",
}
