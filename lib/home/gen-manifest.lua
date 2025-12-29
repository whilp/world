local unix = require("cosmo.unix")
local path = require("cosmo.path")

local home = require("main")

local function walk_dir(dir, base, files)
  files = files or {}
  base = base or ""

  for name in unix.opendir(dir) do
    if name ~= "." and name ~= ".." then
      local full_path = path.join(dir, name)
      local rel_path = base == "" and name or path.join(base, name)
      local st = unix.stat(full_path)

      if st then
        if unix.S_ISDIR(st:mode()) then
          walk_dir(full_path, rel_path, files)
        elseif unix.S_ISREG(st:mode()) or unix.S_ISLNK(st:mode()) then
          local mode = st:mode() & 0x1ff
          files[rel_path] = { mode = mode }
        end
      end
    end
  end

  return files
end

local function cmd_help()
  io.stderr:write("usage: gen-manifest <directory> [version]\n")
  io.stderr:write("\n")
  io.stderr:write("generates a lua manifest table from directory contents\n")
  io.stderr:write("output is written to stdout\n")
  return 0
end

local function main(args)
  if #args == 0 or args[1] == "help" or args[1] == "--help" then
    return cmd_help()
  end

  local dir = args[1]
  local version = args[2]

  local st = unix.stat(dir)
  if not st or not unix.S_ISDIR(st:mode()) then
    io.stderr:write("error: not a directory: " .. dir .. "\n")
    return 1
  end

  local files = walk_dir(dir)
  local manifest = {
    version = version,
    files = files,
  }
  io.write(home.serialize_table(manifest))
  return 0
end

local M = {
  walk_dir = walk_dir,
  main = main,
}

if not pcall(debug.getlocal, 4, 1) then
  os.exit(main(arg) or 0)
end

return M
