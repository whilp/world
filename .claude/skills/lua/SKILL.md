---
name: lua
description: Write lua scripts and modules following repository conventions. Use for lua code, subprocess spawning, config files, or shell script replacements. Includes patterns for file I/O, command execution, error handling, and module structure.
allowed-tools: [Read, Write, Edit, Bash, Glob, Grep]
---

# Lua coding standards

Write lua code following established repository conventions.

## Project structure

Lua code is organized in two locations:

- **`.local/bin/`** - executable wrappers that call modules
- **`lib/`** - reusable library modules

Each location follows specific patterns described below.

## Module templates

Use templates in `.claude/skills/lua/templates/` as starting points:

- **`executable.lua`** - for scripts in `.local/bin/`
- **`module.lua`** - for library modules in `lib/`
- **`test.lua`** - for test files

## Executable pattern

Executables in `.local/bin/` are thin wrappers:

```lua
#!/usr/bin/env lua
local mymodule = require("mymodule")
os.exit(mymodule.main(arg) or 0)
```

Key points:
- Use `#!/usr/bin/env lua` shebang
- Call module's main function and exit with its return code

## Module pattern

Library modules in `lib/`:

```lua
local cosmo = require("cosmo")
local unix = cosmo.unix

local function helper_function(arg)
  if not arg or arg == "" then
    return nil, "arg cannot be empty"
  end
  return arg
end

local function cmd_help(args)
  io.write("mymodule - description\n")
  io.write("\n")
  io.write("usage: mymodule [command] [args]\n")
  io.write("\n")
  io.write("commands:\n")
  io.write("  help    show this help\n")
  io.write("  run     run the module\n")
  return 0
end

local function cmd_run(args)
  if #args == 0 then
    io.stderr:write("usage: mymodule run <arg>\n")
    return 1
  end

  local result, err = helper_function(args[1])
  if not result then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end

  io.write("success\n")
  return 0
end

local function cmd_unknown(command)
  io.stderr:write("unknown command: " .. command .. "\n")
  io.stderr:write("run 'mymodule help' for usage\n")
  return 1
end

local commands = {
  help = cmd_help,
  run = cmd_run,
}

local function main(args)
  if #args == 0 then
    return cmd_help(args)
  end

  local command = args[1]
  local cmd_args = { unpack(args, 2) }
  local cmd_fn = commands[command]

  if cmd_fn then
    return cmd_fn(cmd_args)
  else
    return cmd_unknown(command)
  end
end

if not pcall(debug.getlocal, 4, 1) then
  local exit_code = main(arg)
  os.exit(exit_code or 0)
end

return {
  main = main,
  helper_function = helper_function,
}
```

Key points:
- Use command dispatch table for subcommands
- Implement `cmd_help` and `cmd_unknown` handlers
- Export functions for testing
- Use conditional execution for direct running
- Return table of exported functions

## Test pattern

Test files verify module behavior:

```lua
local cosmo = require('cosmo')
local unix = cosmo.unix

local mymodule = require("mymodule")

function test_function_returns_expected()
  local result = mymodule.helper_function("input")

  lu.assertTrue(result ~= nil, "should not return nil")
  lu.assertEquals(result, "input", "should return input")
end

function test_function_handles_empty_input()
  local result, err = mymodule.helper_function("")

  lu.assertNil(result, "should return nil on error")
  lu.assertTrue(type(err) == "string", "should return error message")
  lu.assertStrContains(err, "empty", "error should mention empty")
end

function test_main_returns_zero_on_success()
  local exit_code = mymodule.main({"help"})

  lu.assertEquals(exit_code, 0, "should return 0 on success")
end
```

Key points:
- Use `function test_*()` naming convention
- Use `lu.*` assertions (luaunit)
- Test both success and error cases

## Error handling

Use `nil, err` for runtime failures:

```lua
local function read_file(path)
  if not path or path == "" then
    return nil, "path cannot be empty"
  end

  local fd = unix.open(path, unix.O_RDONLY)
  if not fd then
    return nil, "failed to open file: " .. path
  end

  local chunks = {}
  while true do
    local chunk = unix.read(fd, 65536)
    if not chunk or chunk == "" then break end
    table.insert(chunks, chunk)
  end
  unix.close(fd)

  return table.concat(chunks)
end
```

Functions return:
- Success: the actual value or `true`
- Failure: `nil, error_message`

Validate inputs early:

```lua
local function download_file(url, dest_path)
  if not url or url == "" then
    return nil, "url cannot be empty"
  end
  if not dest_path or dest_path == "" then
    return nil, "dest_path cannot be empty"
  end
  -- proceed
end
```

Propagate errors with context:

```lua
local ok, err = download_file(url, path)
if not ok then
  return nil, "failed to download " .. name .. ": " .. err
end
```

Use `pcall` for cleanup guarantees:

```lua
local function download_tool(tool_name, platform, output_dir)
  local archive_path = path.join(output_dir, "archive.tar.gz")
  local cleanup_needed = false

  local ok, result = pcall(function()
    local download_ok, download_err = download_file(config.url, archive_path)
    if not download_ok then
      error(download_err)
    end
    cleanup_needed = true

    local verify_ok, verify_err = verify_sha256(archive_path, config.sha)
    if not verify_ok then
      error(verify_err)
    end

    local extract_ok, extract_err = extract(archive_path, output_dir, config)
    if not extract_ok then
      error(extract_err)
    end
    cleanup_needed = false

    return true
  end)

  if not ok and cleanup_needed then
    pcall(unix.unlink, archive_path)
  end

  if not ok then
    return nil, tostring(result)
  end
  return true
end
```

## File I/O

Use `unix.open`, `unix.read`, `unix.write` for file operations:

```lua
local function write_file(filepath, content)
  local fd = unix.open(filepath, unix.O_CREAT | unix.O_WRONLY | unix.O_TRUNC, 0644)
  if not fd then
    return nil, "failed to open " .. filepath .. " for writing"
  end
  unix.write(fd, content)
  unix.close(fd)
  return true
end
```

Read entire file:

```lua
local function read_file(filepath)
  local fd = unix.open(filepath, unix.O_RDONLY)
  if not fd then
    return nil, "failed to open " .. filepath
  end
  local chunks = {}
  while true do
    local chunk = unix.read(fd, 65536)
    if not chunk or chunk == "" then break end
    table.insert(chunks, chunk)
  end
  unix.close(fd)
  return table.concat(chunks)
end
```

For simple reads, use `cosmo.Slurp`:

```lua
local content = cosmo.Slurp(file_path)
if not content then
  return nil, "failed to read file"
end
```

Write to stdout and stderr:

```lua
io.write(output)
io.stderr:write("error: " .. msg .. "\n")
```

## Command execution

Use the `spawn` module for subprocess execution:

```lua
local spawn = require("spawn").spawn

local function execute(program, args)
  local handle, err = spawn(args)
  if not handle then
    return nil, string.format("command failed to start: %s (%s)", program, err or "unknown error")
  end
  local exit_code, wait_err = handle:wait()
  if not exit_code then
    return nil, string.format("command failed: %s (%s)", program, wait_err or "abnormal termination")
  end
  if exit_code ~= 0 then
    return nil, string.format("command failed: %s (exit: %d)", program, exit_code)
  end
  return true
end

local ok, err = execute("tar", {"tar", "-xzf", archive, "-C", output_dir, "--strip-components=1"})
if not ok then
  return nil, err
end
```

Capture output:

```lua
local handle = spawn({"git", "status"})
local success, output, exit_code = handle:read()
if not success then
  return nil, "git status failed: " .. tostring(exit_code)
end
```

Key points:
- Use `spawn(argv)` where argv[1] is the command
- Returns handle with `wait()` and `read()` methods
- Avoid shell invocation - pass arguments directly
- Check exit codes explicitly

## HTTP requests

Use `cosmo.Fetch` for HTTP downloads:

```lua
local function download_file(url, dest_path)
  local status, headers, body
  local last_err
  local max_attempts = 8
  local fetch_opts = {
    headers = {["User-Agent"] = "curl/8.0"},
    maxresponse = 300 * 1024 * 1024
  }

  for attempt = 1, max_attempts do
    status, headers, body = cosmo.Fetch(url, fetch_opts)
    if status then
      break
    end
    last_err = tostring(headers or "unknown error")
    if attempt < max_attempts then
      local delay = math.min(30, 2 ^ attempt)
      unix.nanosleep(delay, 0)
    end
  end

  if not status then
    return nil, "fetch failed: " .. last_err
  end
  if status ~= 200 then
    return nil, "fetch failed with status " .. tostring(status)
  end

  local fd = unix.open(dest_path, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, 0644)
  if not fd or fd < 0 then
    return nil, "failed to open destination file"
  end
  unix.write(fd, body)
  unix.close(fd)

  return true
end
```

Key points:
- Use retry logic with exponential backoff
- Set User-Agent header
- Set maxresponse for large files
- Check status code
- Write body to file with unix.write

## File system operations

Use `cosmo.unix` functions:

```lua
local unix = cosmo.unix

unix.makedirs(dir_path)

unix.rmrf(path)

unix.unlink(file_path)

unix.rename(old_path, new_path)

unix.chmod(file_path, 0755)

local stat = unix.stat(path)
if stat and unix.S_ISDIR(stat:mode()) then
  -- is directory
end

for name in unix.opendir(dir_path) do
  if name ~= "." and name ~= ".." then
    -- process entry
  end
end
```

## Path operations

Use `cosmo.path`:

```lua
local path = cosmo.path

local dir = path.dirname("/foo/bar/baz.txt")

local joined = path.join("foo", "bar", "baz.txt")
```

## String operations

Template interpolation:

```lua
local function interpolate(template, vars)
  if type(template) ~= "string" then
    return template
  end
  return template:gsub("{([%w_]+)}", function(key)
    return tostring(vars[key] or "")
  end)
end

local url = interpolate("https://github.com/{owner}/{repo}/archive/{tag}.tar.gz", {
  owner = "anthropics",
  repo = "cosmo",
  tag = "v1.0.0",
})
```

Pattern matching:

```lua
local owner, repo = url:match("github%.com/([^/]+)/([^/]+)$")
local basename = path:match("([^/]+)$")
```

## Cryptographic operations

Use `cosmo` functions:

```lua
local function verify_sha256(file_path, expected_sha)
  local content = cosmo.Slurp(file_path)
  if not content then
    return nil, "failed to read file"
  end

  local actual = cosmo.EncodeHex(cosmo.Sha256(content)):lower()
  if actual == expected_sha:lower() then
    return true
  end

  return nil, string.format("sha256 mismatch: expected %s, got %s", expected_sha, actual)
end
```

## Configuration files

Pure data tables:

```lua
return {
  name = "tool",
  version = "1.0.0",
  url = "https://example.com/{version}/tool-{platform}.tar.gz",
  platforms = {
    ["darwin-arm64"] = {
      platform = "darwin-arm64",
      sha = "abc123...",
    },
    ["linux-x86_64"] = {
      platform = "linux-x86_64",
      sha = "def456...",
    },
  },
}
```

Template variables use `{variable}` syntax and are interpolated at runtime.

## Common libraries

Standard libraries used in this repository:

- `cosmo` - unified interface to system calls, path utilities, crypto, networking
- `cosmo.unix` - unix system calls (fork, exec, pipe, read, write, stat, etc)
- `cosmo.path` - path manipulation (dirname, join, etc)
- `spawn` - simplified subprocess execution (from `lib/spawn`)

For detailed API documentation, see reference files in this skill directory:

- `cosmo.md` - main module
- `cosmo-unix.md` - unix system calls
- `cosmo-path.md` - path manipulation
- `cosmo-re.md` - regular expressions
- `cosmo-argon2.md` - argon2 password hashing
- `cosmo-sqlite3.md` - sqlite database

## Design principles

**Single responsibility**: Split large functions into focused functions.

**Early validation**: Validate inputs at function boundaries.

**Transaction pattern**: Use pcall with cleanup for state-modifying operations.

**Eliminate duplication**: Create helpers for repeated patterns.

**Minimal comments**: Code should be self-documenting. Only add comments where logic is not self-evident.
