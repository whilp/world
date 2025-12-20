---
name: lua
description: Write lua or LuaJIT scripts and modules following repository conventions. Use for lua or luajit code, posix system calls, subprocess spawning, ffi bindings, config files, or shell script replacements. Includes patterns for file I/O, command execution, bidirectional pipes, error handling, and module structure.
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
#!/usr/bin/env lua

local function main()
  -- implementation
end

main()
```

Key patterns:
- Shebang: `#!/usr/bin/env lua` for executable scripts only
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
    unistd.unlink(temp) -- cleanup on failure
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

IMPORTANT: Always prefer `posix.popen()`, `posix.spawn()`, or `luachild` over `io.popen()` or `os.execute()`:
- `posix.popen()` and `luachild` take an array of arguments and do direct exec without shell invocation
- `io.popen()` invokes `/bin/sh` to parse the command string
- Avoid shell invocation unless you specifically need shell features

### Simple execution with exit code

Use `posix.spawn` for simple commands where you only need the exit status:

``` lua
local posix = require("posix")
local exit_status = posix.spawn({ "command", "arg1", "arg2" })
if exit_status ~= 0 then
  error("command failed")
end
```

### Capture output (read-only)

Use `posix.popen` for running commands with output capture:

```lua
local posix = require('posix')
local unistd = require('posix.unistd')
local wait = require('posix.sys.wait')

local handle = posix.popen({"command", "arg1", "arg2"}, "r")
local output = unistd.read(handle.fd, 65536) or ""
wait.wait(handle.pids[1])
unistd.close(handle.fd)
```

### Bidirectional communication with pipes

Use `luachild` when you need to:
- Send input to stdin and read from stdout/stderr
- Control stdin, stdout, and stderr independently
- Communicate bidirectionally with a subprocess

```lua
local lc = require('luachild')
local unistd = require('posix.unistd')

-- Create pipes for stdin/stdout
local stdin_r, stdin_w = lc.pipe()
local stdout_r, stdout_w = lc.pipe()

-- Spawn process with redirected pipes
local pid = lc.spawn({
  file = '/usr/bin/command',
  args = {'command', 'arg1'},
  env = lc.environ(),
  stdin = stdin_r,
  stdout = stdout_w,
  stderr = stdout_w,  -- redirect stderr to stdout
})

-- Close unused pipe ends in parent
unistd.close(stdin_r)
unistd.close(stdout_w)

-- Write to child's stdin
unistd.write(stdin_w, "input data\n")
unistd.close(stdin_w)

-- Read from child's stdout
local output = unistd.read(stdout_r, 65536) or ""
unistd.close(stdout_r)

-- Wait for child to complete
local status = lc.wait(pid)
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
- `posix.unistd` - Unix standard functions (read, write, close, unlink, exec)
- `posix.sys.wait` - Process waiting
- `posix.signal` - Signal handling
- `luachild` - Subprocess spawning with bidirectional pipe communication
- `ffi` - Foreign function interface (use sparingly, prefer POSIX bindings)
- `openssl.digest` - Hash functions (SHA256)

## POSIX system calls

**IMPORTANT**: Always prefer POSIX bindings over FFI when available. Use FFI only when POSIX bindings don't exist.

Common POSIX operations:

```lua
local posix = require("posix")
local unistd = require("posix.unistd")

-- Create symbolic link (third argument true = symbolic, false = hard)
local result, err = posix.link(target_path, link_path, true)
if result ~= 0 then
  error("symlink failed: " .. tostring(err))
end

-- Remove file or symlink
local result, err = unistd.unlink(path)
if result ~= 0 then
  error("unlink failed: " .. tostring(err))
end

-- Create temporary directory (template must end with XXXXXX)
local temp_dir = posix.mkdtemp("/tmp/myapp.XXXXXX")
if not temp_dir then
  error("mkdtemp failed")
end

-- Remove directory
posix.rmdir(path)

-- Set environment variable
posix.setenv("VAR_NAME", "value")

-- Get process ID
local pid = unistd.getpid()

-- Read symbolic link
local target = unistd.readlink(link_path)

-- Execute program (replaces current process)
unistd.exec(program_path, {"arg0", "arg1", "arg2"})
```

FFI usage (only when POSIX bindings unavailable):

**Most common syscalls are available in luaposix**. Use FFI only for truly missing functionality:

``` lua
local ffi = require("ffi")

-- Example: syscall not in luaposix
ffi.cdef([[
  int some_syscall(const char *arg);
]])
ffi.C.some_syscall("value")
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
