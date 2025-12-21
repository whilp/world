# lfs (LuaFileSystem)

Minimal LuaFileSystem compatibility layer implemented as a stub wrapping `cosmo.unix`. Bundled with the lua binary to support luacheck.

## Usage

```lua
local lfs = require("lfs")
```

## Implementation

This is a compatibility stub, not the full LuaFileSystem library. It provides only the subset of functions needed by luacheck, implemented using `cosmo.unix` underneath.

Source: `3p/lua/lfs_stub.lua`

## Functions

### attributes

```lua
attrs = lfs.attributes(filepath)
value = lfs.attributes(filepath, aname)
```

Get file attributes. Returns a table with file information, or a single attribute if `aname` is specified.

**Attributes:**
- `mode` - file type: `"file"`, `"directory"`, or `"other"`
- `size` - file size in bytes (number)
- `modification` - modification time as Unix timestamp (number)

```lua
local attrs = lfs.attributes("/tmp/file.txt")
print(attrs.mode)          -- "file"
print(attrs.size)          -- 1024
print(attrs.modification)  -- 1734825600

-- Get single attribute
local size = lfs.attributes("/tmp/file.txt", "size")
print(size)  -- 1024
```

### currentdir

```lua
dir = lfs.currentdir()
```

Get current working directory.

```lua
local cwd = lfs.currentdir()
print(cwd)  -- "/workspaces/dotfiles"
```

### dir

```lua
iter = lfs.dir(path)
```

Return an iterator function for directory entries. Does not include `.` or `..`.

```lua
for entry in lfs.dir("/tmp") do
  print(entry)
end
```

### mkdir

```lua
success, err = lfs.mkdir(dirname)
```

Create a directory. Returns `true` on success, or `nil, error` on failure.

```lua
local ok, err = lfs.mkdir("/tmp/newdir")
if not ok then
  print("mkdir failed:", err)
end
```

## Limitations

This stub only implements the functions required by luacheck. For full LuaFileSystem functionality, use the native LuaFileSystem library.

**Not implemented:**
- `chdir`, `lock`, `unlock`, `touch`, `rmdir`
- `symlinkattributes`
- Extended attributes (permissions, owner, group, etc.)
- `link`, `setmode`

## Examples

### Check if file or directory

```lua
local function is_directory(path)
  local mode = lfs.attributes(path, "mode")
  return mode == "directory"
end

if is_directory("/tmp") then
  print("is a directory")
end
```

### List directory contents

```lua
local function list_dir(dir)
  for entry in lfs.dir(dir) do
    local path = dir .. "/" .. entry
    local mode = lfs.attributes(path, "mode")
    print(string.format("%-20s %s", entry, mode))
  end
end

list_dir("/tmp")
```

### Get file size

```lua
local function file_size(path)
  return lfs.attributes(path, "size")
end

local size = file_size("/tmp/file.txt")
print("Size:", size, "bytes")
```
