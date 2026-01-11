#!/usr/bin/env lua
-- teal ignore: type annotations needed
-- wrapper for tl gen that handles output directory
--
-- tl gen has a bug where the -o option doesn't work reliably.
-- this wrapper runs tl gen (which creates output next to input),
-- then moves the generated file to the desired output location.
--
-- to avoid race conditions in parallel builds, we use mkdtemp to create
-- a unique temp directory per invocation.

local cosmo = require("cosmo")
local getopt = require("cosmo.getopt")
local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")

local function main(...)
  local args = {...}
  local output_file = nil

  local longopts = {{"output", "required"}}
  local parser = getopt.new(args, "o:", longopts)

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

  -- resolve all paths before chdir (they may be relative)
  -- TODO: use path.is_absolute() or similar instead of match("^/")
  local orig_cwd = unix.getcwd()
  local tl_dir = os.getenv("TL_BIN")
  if not tl_dir:match("^/") then
    tl_dir = path.join(orig_cwd, tl_dir)
  end
  local tl_script = path.join(tl_dir, "tl")
  local cosmic_bin = unix.commandv("cosmic")
  if cosmic_bin and not cosmic_bin:match("^/") then
    cosmic_bin = path.join(orig_cwd, cosmic_bin)
  end

  -- make output path absolute
  if not output_file:match("^/") then
    output_file = path.join(orig_cwd, output_file)
  end

  -- create unique temp dir next to output to avoid race conditions in parallel
  -- builds (multiple main.tl files would all generate main.lua in cwd)
  -- placing it next to output ensures rename works (same filesystem)
  local output_dir = path.dirname(output_file)
  local tmpdir = unix.mkdtemp(path.join(output_dir, ".tl-gen-XXXXXX"))

  -- make input path absolute before chdir
  local abs_input = input_file
  if not input_file:match("^/") then
    abs_input = path.join(orig_cwd, input_file)
  end

  -- chdir to temp dir so tl gen outputs there
  unix.chdir(tmpdir)

  -- run tl via cosmic (not directly) because tl's shebang uses /usr/bin/env lua
  -- which may find wrong lua; set LUA_PATH so tl.lua module is found
  local lua_path = tl_dir .. "/?.lua;;"
  local env = {}
  for _, entry in ipairs(unix.environ()) do
    if not entry:match("^LUA_PATH=") then
      table.insert(env, entry)
    end
  end
  table.insert(env, "LUA_PATH=" .. lua_path)
  local handle = spawn({ cosmic_bin, tl_script, "gen", abs_input }, { env = env })
  if handle.stdin then
    handle.stdin:close()
  end
  local stdout = handle.stdout and handle.stdout:read() or ""
  local stderr = handle.stderr and handle.stderr:read() or ""
  local exit_code = handle:wait()

  -- restore original cwd
  unix.chdir(orig_cwd)

  if exit_code ~= 0 then
    io.stderr:write(stderr)
    unix.rmrf(tmpdir)
    return exit_code
  end

  -- tl gen creates output in cwd (tmpdir) with just the basename
  local input_basename = path.basename(input_file)
  local generated_name = input_basename:gsub("%.tl$", ".lua")
  local generated_file = path.join(tmpdir, generated_name)

  -- rename to final location (same filesystem, so this always works)
  local ok, err = os.rename(generated_file, output_file)
  if not ok then
    io.stderr:write("failed to rename " .. generated_file .. " to " .. output_file .. ": " .. tostring(err) .. "\n")
    unix.rmrf(tmpdir)
    return 1
  end

  unix.rmrf(tmpdir)
  io.write(stdout)
  return 0
end

if cosmo.is_main() then
  local code = main(...)
  os.exit(code)
end
