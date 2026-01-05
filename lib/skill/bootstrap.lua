-- bootstrap - setup environment for Claude Code web

local unix = require("cosmo.unix")

local CONTEXT = [[
# cosmic: cosmopolitan lua runtime

cosmic is a portable lua distribution with bundled libraries for HTTP, JSON, crypto, compression, and system programming.

## Getting help

- `cosmic --help` - show available options
- `cosmic --help <module>` - get help for a specific module (if available)
- Read lib/ source code for implementation details

## Key modules

**cosmo** - core functions: Fetch (HTTP), EncodeJson/DecodeJson, crypto (Sha256, Md5), compression (Deflate, Inflate), and encoding utilities

**cosmo.path** - path operations: join, basename, dirname, exists, isdir, isfile, islink

**cosmo.unix** - comprehensive POSIX API with all unix syscalls, constants, and file operations

**cosmic.spawn** - process spawning: `spawn({"cmd", "arg"}):wait()` (NEVER use os.execute or io.popen)

## Skills

Use `cosmic --skill <name>` to run skill modules:

**pr** - manages GitHub PRs from .github/pr/<name>.md files. Add `x-cosmic-pr-name: <file>.md` trailer to commits, then run `cosmic --skill pr` to update PR title/description. See `cosmic --skill pr --help` for details.

## Constraints

- NEVER use string concatenation for paths - use `path.join()`
- NEVER use `os.execute()` or `io.popen()` - use `spawn` module
- File permissions: use `tonumber("644", 8)` not `0644` (no octal in lua)
- NEVER manipulate `package.path`
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
