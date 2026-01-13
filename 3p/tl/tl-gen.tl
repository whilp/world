#!/usr/bin/env lua
-- wrapper for tl gen that handles output directory
--
-- tl gen has a bug where the -o option doesn't work reliably.
-- this wrapper runs tl gen (which creates output next to input),
-- then moves the generated file to the desired output location.

local cosmo = require("cosmo")
local getopt = require("cosmo.getopt")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local record SpawnHandle
  stdin: file
  stdout: file
  stderr: file
  wait: function(self: SpawnHandle): number
end

local record GetoptParser
  next: function(self: GetoptParser): string, string
  remaining: function(self: GetoptParser): {string}
end

local function main(...: string): number
  local args = {...}
  local output_file: string = nil

  local longopts = {{"output", "required"}}
  local parser = getopt.new(args, "o:", longopts) as GetoptParser

  while true do
    local opt, optarg = parser:next()
    if not opt then break end
    if opt == "o" or opt == "output" then
      output_file = optarg
    elseif opt == "?" then
      io.stderr:write("usage: tl-gen.lua -o OUTPUT INPUT\n")
      return 1
    end
  end

  local remaining = parser:remaining()
  local input_file = remaining and remaining[1]

  if not input_file or not output_file then
    io.stderr:write("usage: tl-gen.lua -o OUTPUT INPUT\n")
    return 1
  end

  local tl_bin = path.join(os.getenv("TL_BIN") as string, "tl")

  -- run tl gen from the repo root (where tlconfig.lua is)
  -- tl gen creates the output file in cwd with just the basename
  local handle = spawn({ tl_bin, "gen", input_file }) as SpawnHandle
  if handle.stdin then
    handle.stdin:close()
  end
  local stdout = handle.stdout and handle.stdout:read() or ""
  local stderr = handle.stderr and handle.stderr:read() or ""
  local exit_code = handle:wait()

  if exit_code ~= 0 then
    io.stderr:write(stderr as string)
    return exit_code
  end

  -- tl gen creates output in cwd with just the basename
  local input_basename = path.basename(input_file)
  local generated_file = input_basename:gsub("%.tl$", ".lua")
  if generated_file == output_file then
    -- already in the right place
    return 0
  end

  -- move the generated file to the output location
  local ok, _ = os.rename(generated_file, output_file)
  if not ok then
    -- rename failed (maybe cross-device), try copy + remove
    local src = io.open(generated_file, "rb")
    if not src then
      io.stderr:write("failed to open generated file: " .. generated_file .. "\n")
      return 1
    end
    local content = src:read("*a")
    src:close()

    local dst = io.open(output_file, "wb")
    if not dst then
      io.stderr:write("failed to create output file: " .. output_file .. "\n")
      return 1
    end
    dst:write(content)
    dst:close()

    os.remove(generated_file)
  end

  return 0
end

if cosmo.is_main() then
  local code = main(...)
  os.exit(code)
end
