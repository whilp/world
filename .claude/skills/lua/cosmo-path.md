# cosmo.path

Path manipulation utilities for Lua, provided by the `cosmo` module.

## Usage

```lua
local cosmo = require("cosmo")
local path = cosmo.path
```

## Functions

### basename

```lua
name = path.basename(path)
```

Extract filename from path.

```lua
path.basename("/foo/bar/baz.txt")  -- returns "baz.txt"
path.basename("/foo/bar/")         -- returns "bar"
path.basename("file.txt")          -- returns "file.txt"
```

### dirname

```lua
dir = path.dirname(path)
```

Extract directory portion from path.

```lua
path.dirname("/foo/bar/baz.txt")  -- returns "/foo/bar"
path.dirname("/foo/bar/")         -- returns "/foo/bar"
path.dirname("file.txt")          -- returns "."
```

### join

```lua
result = path.join(part1, part2, ...)
```

Join path components with `/` separator. Accepts variable number of arguments.

```lua
path.join("foo", "bar", "baz")     -- returns "foo/bar/baz"
path.join("/usr", "local", "bin")  -- returns "/usr/local/bin"
path.join(".", "file.txt")         -- returns "./file.txt"
```

### exists

```lua
bool = path.exists(path)
```

Test if path exists (file, directory, or other).

```lua
if path.exists("/tmp/file.txt") then
  print("file exists")
end
```

### isfile

```lua
bool = path.isfile(path)
```

Test if path exists and is a regular file.

```lua
if path.isfile("config.lua") then
  print("is a file")
end
```

### isdir

```lua
bool = path.isdir(path)
```

Test if path exists and is a directory.

```lua
if path.isdir("/tmp") then
  print("is a directory")
end
```

### islink

```lua
bool = path.islink(path)
```

Test if path exists and is a symbolic link.

```lua
if path.islink("/usr/bin/lua") then
  print("is a symlink")
end
```

## Examples

### Safely join paths

```lua
local config_dir = os.getenv("HOME")
local config_file = path.join(config_dir, ".config", "app", "config.lua")

if path.exists(config_file) then
  dofile(config_file)
end
```

### Find files in directory

```lua
local dir = "/tmp"
if path.isdir(dir) then
  for name, kind in unix.opendir(dir) do
    local full_path = path.join(dir, name)
    if path.isfile(full_path) then
      print("file: " .. name)
    end
  end
end
```

### Get parent directory

```lua
local script_path = debug.getinfo(1, "S").source:sub(2)
local script_dir = path.dirname(script_path)
local parent_dir = path.dirname(script_dir)
```
