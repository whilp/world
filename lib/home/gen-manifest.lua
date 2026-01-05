-- teal ignore: type annotations needed
local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local walk = require("cosmic.walk")

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

  local files = walk.collect_all(dir)
  local manifest = {
    version = version,
    files = files,
  }
  io.write("return " .. cosmo.EncodeLua(manifest, {pretty = true}) .. "\n")
  return 0
end

local M = {
  main = main,
}

if cosmo.is_main() then
  os.exit(main(arg) or 0)
end

return M
