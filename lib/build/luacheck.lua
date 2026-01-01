#!/usr/bin/env lua
local args = { ... }
local source_file, output, luacheck_bin = args[1], args[2], args[3]

local unix = require("cosmo.unix")
local path = require("cosmo.path")
local cosmo = require("cosmo")
local spawn = require("spawn").spawn

if not source_file or not output or not luacheck_bin then
  io.stderr:write("usage: luacheck.lua <source_file> <output> <luacheck_bin>\n")
  os.exit(1)
end

local output_dir = path.dirname(output)
unix.makedirs(output_dir)

print("# luacheck " .. source_file)

local handle = spawn({
  luacheck_bin,
  "--no-color",
  "--codes",
  "--ranges",
  "--include-files", source_file,
  "--", source_file,
})

-- read() returns: success, stdout, exit_code
local _, stdout, exit_code = handle:read()

if stdout and #stdout > 0 then
  io.write(stdout)
end

local passed = (exit_code == 0)

local result = {
  file = source_file,
  checker = "luacheck",
  passed = passed,
  exit_code = exit_code,
}

local f = io.open(output, "w")
f:write("return " .. cosmo.EncodeLua(result) .. "\n")
f:close()

if not passed then
  os.exit(1)
end
