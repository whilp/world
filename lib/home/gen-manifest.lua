local cosmo = require("cosmo")
local unix = cosmo.unix
local path = cosmo.path

local function walk_dir(dir, base, files)
  files = files or {}
  base = base or ""

  for name in unix.opendir(dir) do
    if name ~= "." and name ~= ".." then
      local full_path = path.join(dir, name)
      local rel_path = base == "" and name or (base .. "/" .. name)
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

local function format_manifest(files, version)
  local lines = {}
  table.insert(lines, "return {")
  if version then
    table.insert(lines, string.format('  version = "%s",', version))
  end
  table.insert(lines, "  files = {")

  local paths = {}
  for p in pairs(files) do
    table.insert(paths, p)
  end
  table.sort(paths)

  for _, p in ipairs(paths) do
    local info = files[p]
    table.insert(lines, string.format('    ["%s"] = { mode = %d },', p, info.mode))
  end

  table.insert(lines, "  },")
  table.insert(lines, "}")
  return table.concat(lines, "\n")
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
  local manifest = format_manifest(files, version)
  io.write(manifest .. "\n")
  return 0
end

local M = {
  walk_dir = walk_dir,
  format_manifest = format_manifest,
  main = main,
}

if not pcall(debug.getlocal, 4, 1) then
  os.exit(main(arg) or 0)
end

return M
