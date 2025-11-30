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
- Local helper functions: `local function name(...)`

## Error handling

Use `nil, err` for runtime failures, reserve `error()` for programmer errors:

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

Functions return:
- Success: `true` or the actual value
- Failure: `nil, error_message`

Use `error()` only for unrecoverable programmer errors:

``` lua
if not config.name then
  error("config missing required field 'name'")
end
```

Validate inputs early:

``` lua
local function download_file(url, dest_path)
  if not url or url == "" then
    return nil, "url cannot be empty"
  end
  -- proceed with download
end
```

Propagate errors with context:

``` lua
local ok, err = download_file(url, download_path)
if not ok then
  return nil, "failed to download " .. name .. ": " .. err
end
```

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
    return process(temp_dir)
  end)
  file.rm_rf(temp_dir) -- cleanup always happens
  if not ok then
    return nil, result
  end
  return result
end
```

## File I/O

Standard write pattern:

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

Atomic operations (temp file + rename) for critical writes:

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
- Avoid shell invocation unless you specifically need shell features

Use `posix.popen` for running commands with output:

```lua
local posix = require('posix')
local unistd = require('posix.unistd')
local wait = require('posix.sys.wait')

local handle = posix.popen({"command", "arg1", "arg2"}, "r")
local output = unistd.read(handle.fd, 65536) or ""
wait.wait(handle.pids[1])
unistd.close(handle.fd)
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

## String and table operations

Pattern matching:

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
  platforms = {
    ["darwin-arm64"] = {
      arch = "darwin-arm64",
      sha256 = "abc123...",
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
- `ffi` - Foreign function interface for C calls
- `openssl.digest` - Hash functions (SHA256)

FFI usage for C functions:

``` lua
local ffi = require("ffi")
ffi.cdef([[
  int symlink(const char *target, const char *linkpath);
]])
local result = ffi.C.symlink(target, linkpath)
```

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

## Design principles

**Single responsibility**: Split large functions into focused functions with single responsibilities.

**Transaction pattern**: Use pcall with guaranteed cleanup for operations that modify state:

``` lua
local function install_package(name, config)
  local temp_dir = create_temp_dir()
  local ok, result = pcall(function()
    download_to(temp_dir)
    validate_checksums(temp_dir)
    return atomic_move_to_final(temp_dir, final_dir)
  end)
  file.rm_rf(temp_dir) -- always cleanup
  if not ok then
    return nil, result
  end
  return result
end
```

**Early validation**: Validate inputs at function boundaries with descriptive errors.

**Eliminate duplication**: Create helpers for repeated patterns instead of copy-paste.

## Signal handling

Register cleanup handlers for graceful shutdown:

``` lua
local signal = require("posix.signal")
local cleanup_registry = {}

local function cleanup_temp_resources()
  for _, path in ipairs(cleanup_registry) do
    if path and file.exists(path) then
      file.rm_rf(path)
    end
  end
end

local function setup_signal_handlers()
  local handler = function(signum)
    cleanup_temp_resources()
    os.exit(128 + signum)
  end
  signal.signal(signal.SIGINT, handler)
  signal.signal(signal.SIGTERM, handler)
end

setup_signal_handlers()
```
