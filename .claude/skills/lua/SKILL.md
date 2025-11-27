---
name: lua
description: Write LuaJIT scripts and modules following repository conventions. Use for lua/luajit code, posix system calls, ffi bindings, config files, or shell script replacements. Includes patterns for file I/O, command execution, error handling, and module structure.
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

```lua
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

Use `pcall` for optional dependencies:

```lua
local ok, module = pcall(require, "module_name")
if ok then
  -- use module
end
```

Use guard clauses for validation:

```lua
local function read_file(path)
  local f = io.open(path, "r")
  if not f then
    return nil
  end
  local content = f:read("*all")
  f:close()
  return content
end
```

Use explicit error messages:

```lua
if not condition then
  error("descriptive error message")
end
```

## File I/O patterns

Standard read pattern:

```lua
local f = io.open(path, "r")
if not f then
  return nil
end
local content = f:read("*all")  -- or "*l" for single line
f:close()
return content
```

Standard write pattern:

```lua
local f = io.open(path, "w")
if not f then
  return false
end
f:write(content)
f:close()
return true
```

Use atomic operations (temp file + rename) for critical writes:

```lua
local temp = path .. ".tmp." .. os.time()
local f = io.open(temp, "w")
if not f then
  return false
end
f:write(content)
f:close()
os.rename(temp, path)
```

Write to stdout and stderr:

```lua
io.write(output)  -- stdout, no newline
io.stderr:write("error: " .. msg .. "\n")
io.stderr:flush()  -- ensure immediate output
```

## Command execution

Use `posix.spawn` for simple execution:

```lua
local posix = require('posix')
local exit_status = posix.spawn({"command", "arg1", "arg2"})
if exit_status ~= 0 then
  error("command failed")
end
```

Use `posix.popen` for capturing output:

```lua
local posix = require('posix')
local unistd = require('posix.unistd')
local wait = require('posix.sys.wait')

local handle = posix.popen({"command", "arg"}, "r")
local output = unistd.read(handle.fd, 65536) or ""
wait.wait(handle.pids[1])
unistd.close(handle.fd)
```

## Command-line arguments

Subcommand pattern:

```lua
local function main(args)
  if #args == 0 or args[1] == "help" then
    print("usage: command [subcommand]")
    os.exit(0)
  end

  local command = args[1]
  local cmd_args = {unpack(args, 2)}

  if command == "show" then
    cmd_show(cmd_args)
  elseif command == "write" then
    cmd_write(cmd_args)
  else
    error("unknown command: " .. command)
  end
end

local args = {...}
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

## String operations

Use pattern matching:

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

## Table operations

Recursive table expansion:

```lua
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
  path = "bin/${name}",
  url = "https://example.com/${version}/${name}.tar.gz",
  platforms = {
    ["darwin-arm64"] = {
      arch = "darwin-arm64",
      sha256 = "abc123..."
    },
    ["linux-x86_64"] = {
      arch = "linux-x64",
      sha256 = "def456..."
    }
  }
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

```lua
local ffi = require("ffi")
ffi.cdef([[
  int symlink(const char *target, const char *linkpath);
]])
local result = ffi.C.symlink(target, linkpath)
```

## Commenting conventions

Remove comments unless they're very useful:

```lua
-- Example: flatten({{1, 2}, {3, 4}}) -> {1, 2, 3, 4}
M.flatten = function(tbl)
  -- implementation
end
```

General-purpose library modules may include a brief header comment explaining the module's purpose.

## Platform detection

```lua
local function get_platform()
  local system = jit.os:lower()
  local machine = jit.arch:lower()

  local system_map = { osx = "darwin" }
  system = system_map[system] or system

  return system .. "-" .. machine
end
```

## Best practices

1. Use guard clauses and early returns
2. Prefer explicit over implicit (avoid colon syntax for methods)
3. Use atomic operations for critical file writes
4. Always close file handles
5. Check for nil before using file handles
6. Use `pcall` to wrap main entry points
7. Write errors to stderr: `io.stderr:write()`
8. Keep modules focused and single-purpose
9. Remove comments unless they're very useful
10. Store library code in `~/.local/lib/lua/`
11. Store executable scripts in `~/.local/bin/`
