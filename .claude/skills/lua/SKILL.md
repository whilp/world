---
name: lua
description: Write lua or LuaJIT scripts and modules following repository conventions. Use for lua or luajit code, posix system calls, ffi bindings, config files, or shell script replacements. Includes patterns for file I/O, command execution, error handling, and module structure.
allowed-tools: [Read, Write, Edit, Bash, Glob, Grep]
---

# Lua coding standards

Write Lua code following established repository conventions for consistency and maintainability.

## Module structure

Library module pattern (in `~/.local/lib/lua/`):

```lua
local M = {}

M.function_name = function(arg)
  -- implementation
end

return M
```

Executable script pattern (in `~/.local/bin/`):

``` lua
#!/usr/bin/env luajit

local function main()
  -- implementation
end

main()
```

Key patterns:

- Shebang: `#!/usr/bin/env luajit` for executable scripts only
- Module table: `local M = {}` ... `return M`
- Function definitions: `M.name = function(...)` or `function M.name()` both acceptable
- Custom library path: Only in executable scripts, not in library modules
- Local helper functions: `local function name(...)`

## Error handling

### Consistent error pattern

Use `nil, err` for runtime failures, reserve `error()` for programmer errors:

``` lua
local function read_file(path)
  local f = io.open(path, "r")
  if not f then
    return nil, "failed to open file: " .. path
  end
  local content = f:read("*all")
  f:close()
  return content
end
```

Functions return:

- Success: `true` or the actual value
- Failure: `nil, error_message`

Use `error()` only for unrecoverable programmer errors:

``` lua
if not config.name then
  error("config missing required field 'name'")
end
```

### Input validation

Validate inputs early with descriptive errors:

``` lua
local function download_file(url, dest_path)
  if not url or url == "" then
    return nil, "url cannot be empty"
  end
  if not dest_path or dest_path == "" then
    return nil, "dest_path cannot be empty"
  end
  -- proceed with download
end
```

### Error propagation

Propagate errors with context:

``` lua
local function download_to_temp(url, name, temp_dir)
  local download_path = file.path_join(temp_dir, file.basename(url))
  local ok, err = download_file(url, download_path)
  if not ok then
    return nil, "failed to download " .. name .. ": " .. err
  end
  return download_path
end
```

### Protected calls

Use `pcall` for optional dependencies and cleanup:

``` lua
local ok, module = pcall(require, "module_name")
if ok then
  -- use module
end
```

Wrap operations needing guaranteed cleanup:

``` lua
local function operation(path)
  local temp_dir = create_temp_dir()
  local ok, result = pcall(function()
    -- risky operations
    return process(temp_dir)
  end)
  file.rm_rf(temp_dir) -- cleanup always happens
  if not ok then
    return nil, result -- result is error message
  end
  return result
end
```

## File I/O patterns

Standard read pattern with error handling:

``` lua
local function read_file(path)
  local f = io.open(path, "r")
  if not f then
    return nil, "failed to open file: " .. path
  end
  local content = f:read("*all") -- or "*l" for single line
  f:close()
  return content
end
```

Standard write pattern with error handling:

``` lua
local function write_file(path, content)
  local f = io.open(path, "w")
  if not f then
    return nil, "failed to open file for writing: " .. path
  end
  f:write(content)
  f:close()
  return true
end
```

Use atomic operations (temp file + rename) for critical writes:

``` lua
local function atomic_write(path, content)
  local unistd = require("posix.unistd")
  local temp = string.format("%s.tmp.%d.%d", path, os.time(), unistd.getpid())

  local f = io.open(temp, "w")
  if not f then
    return nil, "failed to create temp file: " .. temp
  end
  f:write(content)
  f:close()

  local ok, err = os.rename(temp, path)
  if not ok then
    ffi.C.unlink(temp) -- cleanup on failure
    return nil, "failed to rename temp file: " .. err
  end
  return true
end
```

Write to stdout and stderr:

``` lua
io.write(output) -- stdout, no newline
io.stderr:write("error: " .. msg .. "\n")
io.stderr:flush() -- ensure immediate output
```

## Command execution

IMPORTANT: Always prefer `posix.popen()` or `posix.spawn()` over `io.popen()` or `os.execute()`:
- `posix.popen()` takes an array of arguments and does direct exec without shell invocation
- `io.popen()` invokes `/bin/sh` to parse the command string
- Avoid shell invocation unless you specifically need shell features (sourcing files, variable expansion, etc.)

Use `posix.popen` for running commands (with or without capturing output):

```lua
local posix = require('posix')
local unistd = require('posix.unistd')
local wait = require('posix.sys.wait')

local handle = posix.popen({"command", "arg1", "arg2"}, "r")
local output = unistd.read(handle.fd, 65536) or ""
wait.wait(handle.pids[1])
unistd.close(handle.fd)
```

For fire-and-forget commands (no output needed):

```lua
local function run_command(args)
  local posix = require("posix")
  local handle = posix.popen(args, "r")
  if handle then
    wait.wait(handle.pids[1])
    unistd.close(handle.fd)
  end
end

run_command({"systemctl", "--user", "restart", "service"})
```

Use `posix.spawn` for simple execution with exit code:

``` lua
local posix = require("posix")
local exit_status = posix.spawn({ "command", "arg1", "arg2" })
if exit_status ~= 0 then
  error("command failed")
end
```
## Command-line arguments

Subcommand pattern:

``` lua
local function main(args)
  if #args == 0 or args[1] == "help" then
    print("usage: command [subcommand]")
    os.exit(0)
  end

  local command = args[1]
  local cmd_args = { unpack(args, 2) }

  if command == "show" then
    cmd_show(cmd_args)
  elseif command == "write" then
    cmd_write(cmd_args)
  else
    error("unknown command: " .. command)
  end
end

local args = { ... }
local ok, err = pcall(main, args)
if not ok then
  io.stderr:write("error: " .. tostring(err) .. "\n")
  os.exit(1)
end
```

Parse key=value parameters:

``` lua
local function parse_params(args)
  local params = {}
  for _, arg in ipairs(args) do
    local key, value = arg:match("^([^=]+)=(.+)$")
    if key and value then
      params[key] = value
    end
  end
  return params
end
```

## String operations

Use pattern matching:

``` lua
local basename = path:match("([^/]+)$")
local key, value = arg:match("^([^=]+)=(.+)$")
```

Template interpolation:

``` lua
M.interpolate = function(template, context)
  if type(template) ~= "string" then
    return template
  end
  return template:gsub("%${([%w_]+)}", function(key)
    return tostring(context[key] or "")
  end)
end
```

## Table operations

Recursive table expansion:

``` lua
M.expand = function(value, context)
  if type(value) == "string" then
    return M.interpolate(value, context)
  elseif type(value) == "table" then
    local expanded = {}
    for k, v in pairs(value) do
      expanded[k] = M.expand(v, context)
    end
    return expanded
  else
    return value
  end
end
```

Flatten nested tables:

``` lua
M.flatten = function(tbl)
  local result = {}
  for _, item in ipairs(tbl) do
    if type(item) == "table" then
      for _, sub in ipairs(M.flatten(item)) do
        table.insert(result, sub)
      end
    else
      table.insert(result, item)
    end
  end
  return result
end
```

## Configuration files

Pure data tables for config:

``` lua
return {
  name = "tool",
  version = "1.0.0",
  path = "bin/${name}",
  url = "https://example.com/${version}/${name}.tar.gz",
  platforms = {
    ["darwin-arm64"] = {
      arch = "darwin-arm64",
      sha256 = "abc123...",
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha256 = "def456...",
    },
  },
}
```

Template variables use `${variable}` syntax and are interpolated at runtime.

## Common dependencies

Standard libraries used in this repo:

- `posix` - POSIX system calls
- `posix.unistd` - Unix standard functions (read, write, close)
- `posix.sys.wait` - Process waiting
- `posix.sys.stat` - File stats
- `posix.dirent` - Directory operations
- `ffi` - Foreign function interface for C calls
- `openssl` - Cryptography
- `openssl.digest` - Hash functions (SHA256)
- `serpent` - Lua table serialization
- `dkjson` - JSON encoding/decoding (if needed)

FFI usage for C functions:

``` lua
local ffi = require("ffi")
ffi.cdef([[
  int symlink(const char *target, const char *linkpath);
]])
local result = ffi.C.symlink(target, linkpath)
```

## Commenting conventions

Remove comments unless they're very useful:

``` lua
-- Example: flatten({{1, 2}, {3, 4}}) -> {1, 2, 3, 4}
M.flatten = function(tbl)
  -- implementation
end
```

General-purpose library modules may include a brief header comment explaining the module's purpose.

## Platform detection

``` lua
local function get_platform()
  local system = jit.os:lower()
  local machine = jit.arch:lower()

  local system_map = { osx = "darwin" }
  system = system_map[system] or system

  return system .. "-" .. machine
end
```

## Function design principles

### Single responsibility

Split large functions into focused functions with single responsibilities:

``` lua
-- Before: 100+ line monolithic function
local function download_executable(name, config, force)
  -- download, checksum, extract, install all mixed together
end

-- After: Split into focused functions
local function download_to_temp(url, name, temp_dir)
  -- only handles downloading
end

local function validate_checksum(path, expected, name, config, force)
  -- only handles checksum validation
end

local function extract_and_prepare(archive, temp_dir, path, config)
  -- only handles extraction
end

local function install_to_versioned_dir(extract_temp, sha256, config)
  -- only handles installation
end

-- Coordinator function
local function download_executable(name, config, force)
  local temp_dir = create_temp()
  local download_path, err = download_to_temp(config.url, name, temp_dir)
  if not download_path then
    file.rm_rf(temp_dir)
    return nil, err
  end
  -- chain other operations with error handling
end
```

### Eliminate duplication

Create single source of truth for repeated patterns:

``` lua
-- Before: Duplicated path construction
local path1 = path_join("..", "_", name, sha:sub(1, 16), exec_path)
local path2 = path_join("..", "_", name, sha:sub(1, 16), exec_path)

-- After: Single helper function
local function get_relative_versioned_path(name, sha256, exec_path)
  local short_sha = sha256:sub(1, 16)
  if exec_path then
    return path_join("..", "_", name, short_sha, exec_path)
  else
    return path_join("..", "_", name, short_sha)
  end
end
```

### Transaction pattern

Use transaction pattern for operations that modify state:

``` lua
local function install_package(name, config)
  local temp_dir = create_temp_dir()

  -- Wrap risky operations
  local ok, result = pcall(function()
    download_to(temp_dir)
    validate_checksums(temp_dir)
    extract_to(temp_dir)
    -- Only commit at the end
    return atomic_move_to_final(temp_dir, final_dir)
  end)

  -- Always cleanup temp, regardless of success
  file.rm_rf(temp_dir)

  if not ok then
    return nil, result
  end
  return result
end
```

### Validation early

Validate inputs at function boundaries:

``` lua
local function process_config(config)
  -- Validate required fields
  if not config.name or config.name == "" then
    return nil, "config missing required field 'name'"
  end
  if type(config.platforms) ~= "table" then
    return nil, "config field 'platforms' must be table, got " .. type(config.platforms)
  end
  if not next(config.platforms) then
    return nil, "config field 'platforms' cannot be empty"
  end

  -- Process only after validation passes
  return process(config)
end
```

## Signal handling

Register cleanup handlers for graceful shutdown:

``` lua
local signal = require("posix.signal")
local cleanup_registry = {}

local function register_temp_dir(path)
  table.insert(cleanup_registry, path)
end

local function cleanup_temp_resources()
  for _, path in ipairs(cleanup_registry) do
    if path and file.exists(path) then
      file.rm_rf(path)
    end
  end
  cleanup_registry = {}
end

local function setup_signal_handlers()
  local handler = function(signum)
    io.stderr:write(string.format("received signal %d, cleaning up...\n", signum))
    io.stderr:flush()
    cleanup_temp_resources()
    os.exit(128 + signum)
  end

  signal.signal(signal.SIGINT, handler)
  signal.signal(signal.SIGTERM, handler)
end

-- Call at program start
setup_signal_handlers()
```

## Best practices

1. **Error handling**: Use consistent `nil, err` pattern, reserve `error()` for programmer errors
2. **Input validation**: Validate early with descriptive messages
3. **Single responsibility**: Each function should do one thing well
4. **Eliminate duplication**: Create helpers for repeated patterns
5. **Transaction pattern**: Extract to temp, validate, then atomic move
6. **Guard clauses**: Use early returns for validation
7. **Atomic operations**: Use PID in temp file names, cleanup in all paths
8. **Error propagation**: Add context when propagating errors
9. **Signal handling**: Register cleanup handlers for graceful shutdown
10. **Protected calls**: Use `pcall` with guaranteed cleanup
11. **File handles**: Always close, check for nil before using
12. **Module organization**: Keep focused and single-purpose
13. **Code clarity**: Remove comments unless very useful
14. **Standard paths**: Library code in `~/.local/lib/lua/`, executables in `~/.local/bin/`
