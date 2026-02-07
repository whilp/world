local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local fs = require("cosmic.fs")

local record Stat
  mode: function(self): number
  size: function(self): number
  mtim: function(self): number
end

local record FileInfo
  mode: number
end

local function cmd_help(): integer
  io.stderr:write("usage: gen-manifest <directory> [version]\n")
  io.stderr:write("\n")
  io.stderr:write("generates a lua manifest table from directory contents\n")
  io.stderr:write("output is written to stdout\n")
  return 0
end

local record Manifest
  version: string
  files: {string:FileInfo}
end

local function main(args: {string}): integer
  if #args == 0 or args[1] == "help" or args[1] == "--help" then
    return cmd_help()
  end

  local dir = args[1]
  local version = args[2]

  local st = unix.stat(dir) as Stat
  if not st or not unix.S_ISDIR(st:mode()) then
    io.stderr:write("error: not a directory: " .. dir .. "\n")
    return 1
  end

  local files = fs.collect_all(dir) as {string:FileInfo}
  local manifest: Manifest = {
    version = version,
    files = files,
  }
  io.write("return " .. cosmo.EncodeLua(manifest as any) .. "\n")
  return 0
end

local record M
  main: function(args: {string}): integer
end

local M_impl: M = {
  main = main,
}

if cosmo.is_main() then
  os.exit(main(arg) as integer)
end

return M_impl
