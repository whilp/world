---
name: lua
description: Write lua scripts and modules following repository conventions. Use for lua code, subprocess spawning, config files, or shell script replacements. Includes patterns for file I/O, command execution, bidirectional pipes, error handling, and module structure.
allowed-tools: [Read, Write, Edit, Bash, Glob, Grep]
---

# Lua coding standards

Write Lua code following established repository conventions for consistency and maintainability.

## Project structure

This repository organizes Lua code in three locations:

- **`.local/bin/`** - executable wrappers that set up paths and call main modules
- **`src/*/main.lua`** - application logic modules
- **`src/*/*.lua`** - reusable library modules (legacy: `.local/lib/lua/`)

Each location follows specific patterns described below.

## Module templates

Use the templates in `.claude/skills/lua/templates/` as starting points:

- **`executable.lua`** - for scripts in `.local/bin/`
- **`main.lua`** - for application modules in `src/*/main.lua`
- **`module.lua`** - for library modules in `src/*/*.lua` (or `.local/lib/lua/`, legacy)
- **`test.lua`** - for test files in `src/*/test*.lua`

## Executable pattern

Executables in `.local/bin/` are thin wrappers that:
1. Set up the Lua module path to include `src/`
2. Load the corresponding module from `src/*/main.lua`
3. Call the module's main function with conditional execution
4. Return the module for testability

See `.claude/skills/lua/templates/executable.lua` for full template.

Key points:
- Always use `#!/usr/bin/env lua` shebang
- Use `cosmo.path.dirname()` to build paths relative to script location
- Use `pcall(debug.getlocal, 4, 1)` to detect if running as script vs being required
- Pass `arg` table to main function
- Return module to support testing

## Main module pattern

Modules in `src/*/main.lua` contain application logic:

```lua
local cosmo = require("cosmo")
local unix = cosmo.unix

local function helper_function(arg)
  if not arg or arg == "" then
    return nil, "arg cannot be empty"
  end
  -- implementation
  return true
end

local function cmd_help(args)
  io.write("mymodule - description\n")
  io.write("\n")
  io.write("usage: mymodule [command] [args]\n")
  io.write("\n")
  io.write("commands:\n")
  io.write("  help    show this help\n")
  io.write("  run     run the module\n")
  io.write("  env     example with environment variables\n")
  return 0
end

local function cmd_run(args)
  local result, err = helper_function(args[1])
  if not result then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end
  return 0
end

local function cmd_env(args)
  -- Read specific environment variable
  local home = os.getenv("HOME")
  if home then
    io.write("HOME=" .. home .. "\n")
  end

  -- Build custom environment for subprocess
  -- unix.environ() returns array of "KEY=value" strings
  local env = unix.environ()
  table.insert(env, "CUSTOM_VAR=custom_value")

  -- Example: could use env with unix.execve()
  -- unix.execve("/usr/bin/env", {"env"}, env)

  io.write("environment prepared\n")
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
  env = cmd_env,
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

return {
  main = main,
  helper_function = helper_function,
}
```

Key points:
- Use command dispatch table pattern for subcommands
- Implement `cmd_help` and `cmd_unknown` handlers
- Export both `main` and helper functions for testing
- `main` takes args table and returns exit code
- Use `local function` for all functions
- Return table of exported functions

## Library module pattern

Modules in `src/*/*.lua` (or `.local/lib/lua/`, legacy) are reusable libraries:

```lua
local cosmo = require("cosmo")
local unix = cosmo.unix

local function function_name(arg)
  if not arg or arg == "" then
    return nil, "arg cannot be empty"
  end

  -- implementation
  return result
end

local function cmd_help(args)
  io.write("module - description\n")
  io.write("\n")
  io.write("usage: module [command] [args]\n")
  io.write("\n")
  io.write("commands:\n")
  io.write("  help    show this help\n")
  io.write("  run     run the module\n")
  return 0
end

local function cmd_run(args)
  local result, err = function_name(args[1])
  if not result then
    io.stderr:write("error: " .. err .. "\n")
    return 1
  end
  return 0
end

local function cmd_unknown(command)
  io.stderr:write("unknown command: " .. command .. "\n")
  io.stderr:write("run 'module help' for usage\n")
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

local M = {
  function_name = function_name,
  main = main,
}

-- Run main if executed directly (not required as a module)
if not pcall(debug.getlocal, 4, 1) then
  local exit_code = main(arg)
  os.exit(exit_code or 0)
end

return M
```

Key points:
- Use `local function` for all functions
- Use command dispatch table pattern for subcommands
- Implement `cmd_help` and `cmd_unknown` handlers
- Optionally include a `main` function for direct execution
- Assemble table at end with exported functions
- Use conditional execution pattern to run main only when executed directly
- Always return table at end
- Use `nil, err` for error returns

## Test pattern

Test files in `src/*/test.lua` verify module behavior. Package paths are configured in `src/test.mk` via `LUA_PATH` - add a test target there with paths to `.local/lib/lua` and `src` modules:

```lua
local cosmo = require('cosmo')
local unix = cosmo.unix

local mymodule = require("mymodule.main")

function test_function_returns_expected()
  local result = mymodule.helper_function("input")

  lu.assertTrue(type(result) == "string", "should return string")
end

function test_function_handles_error()
  local result, err = mymodule.helper_function("")

  lu.assertNil(result, "should return nil on error")
  lu.assertTrue(type(err) == "string", "should return error message")
end
```

Add to `src/test.mk`:
```make
test-mymodule: lua
	cd src/mymodule && HOME=$(CURDIR) \
		LUA_PATH="$(CURDIR)/.local/lib/lua/?.lua;$(CURDIR)/src/?.lua;;" \
		$(CURDIR)/$(lua_bin) $(CURDIR)/$(test_runner) test.lua
```

Key points:
- Set up package.path to find both lib modules and the module under test
- Use `function test_*()` naming convention
- Use `lu.*` assertions (luaunit)
- Test both success and error cases

## Error handling

Use `nil, err` for runtime failures, reserve `error()` for programmer errors:

```lua
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

```lua
if not config.name then
  error("config missing required field 'name'")
end
```

Validate inputs early:

```lua
local function download_file(url, dest_path)
  if not url or url == "" then
    return nil, "url cannot be empty"
  end
  -- proceed with download
end
```

Propagate errors with context:

```lua
local ok, err = download_file(url, download_path)
if not ok then
  return nil, "failed to download " .. name .. ": " .. err
end
```

Use `pcall` for optional dependencies and cleanup:

```lua
local ok, module = pcall(require, "module_name")
if ok then
  -- use module
end
```

Wrap operations needing guaranteed cleanup:

```lua
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

```lua
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

```lua
local function atomic_write(path, content)
  local unix = require("cosmo").unix
  local temp = string.format("%s.tmp.%d.%d", path, os.time(), unix.getpid())

  local f = io.open(temp, "w")
  if not f then
    return nil, "failed to create temp file: " .. temp
  end
  f:write(content)
  f:close()

  local ok, err = os.rename(temp, path)
  if not ok then
    unix.unlink(temp) -- cleanup on failure
    return nil, "failed to rename temp file: " .. err
  end
  return true
end
```

Write to stdout and stderr:

```lua
io.write(output) -- stdout, no newline
io.stderr:write("error: " .. msg .. "\n")
io.stderr:flush() -- ensure immediate output
```

## Command execution

Use `unix.fork()` and `unix.execve()` for subprocess spawning:
- Takes an array of arguments and does direct exec without shell invocation
- Avoid `io.popen()` which invokes `/bin/sh` to parse the command string
- Only use shell invocation when you specifically need shell features

### Bidirectional communication with pipes

```lua
local unix = require('cosmo').unix

-- Create pipes for stdin/stdout
local stdin_r, stdin_w = unix.pipe()
local stdout_r, stdout_w = unix.pipe()

-- Fork process
local pid = unix.fork()

if pid == 0 then
  -- Child process
  unix.close(stdin_w)
  unix.close(stdout_r)

  -- Redirect stdin and stdout
  unix.dup2(stdin_r, 0)
  unix.dup2(stdout_w, 1)
  unix.dup2(stdout_w, 2)

  unix.close(stdin_r)
  unix.close(stdout_w)

  -- Execute command
  unix.execve('/usr/bin/command', {'command', 'arg1'}, unix.environ())
  unix.exit(1)  -- only reached on error
else
  -- Parent process
  unix.close(stdin_r)
  unix.close(stdout_w)

  -- Write to child's stdin
  unix.write(stdin_w, "input data\n")
  unix.close(stdin_w)

  -- Read from child's stdout
  local output = unix.read(stdout_r, 65536) or ""
  unix.close(stdout_r)

  -- Wait for child to complete
  local status = unix.wait()
end
```

## Command-line arguments

Subcommand dispatch pattern:

```lua
local function cmd_help(args)
  io.write("usage: command [subcommand]\n")
  io.write("\n")
  io.write("commands:\n")
  io.write("  help    show this help\n")
  io.write("  show    show information\n")
  return 0
end

local function cmd_show(args)
  -- implementation
  return 0
end

local function cmd_unknown(command)
  io.stderr:write("unknown command: " .. command .. "\n")
  return 1
end

local commands = {
  help = cmd_help,
  show = cmd_show,
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

local args = { ... }
local ok, err = pcall(main, args)
if not ok then
  io.stderr:write("error: " .. tostring(err) .. "\n")
  os.exit(1)
end
```

Parse key=value parameters:

```lua
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

```lua
local basename = path:match("([^/]+)$")
local key, value = arg:match("^([^=]+)=(.+)$")
```

Template interpolation:

```lua
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

```lua
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

```lua
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
- `cosmo` - Unified interface to Unix system calls and path utilities
- `cosmo.unix` - Unix system calls (fork, exec, pipe, read, write, etc)
- `cosmo.path` - Path manipulation utilities (dirname, etc)
- `daemonize` - Daemon process creation (from `src/` modules)

For detailed API documentation, see the cosmo reference files in this skill directory:
- `cosmo.md` - Main module (encoding, compression, hashing, networking, system info)
- `cosmo-unix.md` - Unix system calls
- `cosmo-path.md` - Path manipulation
- `cosmo-re.md` - Regular expressions
- `cosmo-argon2.md` - Argon2 password hashing
- `cosmo-sqlite3.md` - SQLite database

## Unix system calls

Common operations using `cosmo.unix`:

```lua
local cosmo = require("cosmo")
local unix = cosmo.unix

-- Fork process
local pid = unix.fork()
if pid == 0 then
  -- child process
  unix.exit(0)
elseif pid > 0 then
  -- parent process
  unix.wait()
end

-- Execute program (replaces current process)
unix.execve(program_path, {"arg0", "arg1", "arg2"}, unix.environ())

-- Create pipes
local read_fd, write_fd = unix.pipe()

-- Read/write
local data = unix.read(fd, 65536)
unix.write(fd, "data")
unix.close(fd)

-- File operations
local stat = unix.stat(path)
local fd = unix.open(path, unix.O_RDWR)

-- Signals
unix.sigaction(unix.SIGINT, handler_function)
unix.kill(pid, unix.SIGTERM)

-- Process ID
local pid = unix.getpid()
```

## Design principles

**Single responsibility**: Split large functions into focused functions with single responsibilities.

**Transaction pattern**: Use pcall with guaranteed cleanup for operations that modify state:

```lua
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

```lua
local cosmo = require("cosmo")
local unix = cosmo.unix
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
  unix.sigaction(unix.SIGINT, handler)
  unix.sigaction(unix.SIGTERM, handler)
end

setup_signal_handlers()
```
