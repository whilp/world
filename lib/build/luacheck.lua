#!/usr/bin/env lua
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local cosmo = require("cosmo")
local spawn = require("spawn").spawn

local function check(source_file, output, luacheck_bin)
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

  -- TODO: fail build once all files pass
end

local function report(output_dir)
  local files = {}
  local passed = 0
  local failed = 0

  local handle = spawn({ "find", output_dir, "-name", "*.luacheck.ok", "-type", "f" })
  local _, stdout = handle:read()

  for line in (stdout or ""):gmatch("[^\n]+") do
    local chunk = loadfile(line)
    if chunk then
      local result = chunk()
      if result then
        table.insert(files, result)
        if result.passed then
          passed = passed + 1
        else
          failed = failed + 1
        end
      end
    end
  end

  local total = passed + failed

  print("luacheck report")
  print("───────────────────────────────")
  print(string.format("  total checked:  %d", total))
  print(string.format("  passed:         %d", passed))
  print(string.format("  failed:         %d", failed))
  print("")

  if failed > 0 then
    print("files with issues:")
    table.sort(files, function(a, b) return a.file < b.file end)
    for _, f in ipairs(files) do
      if not f.passed then
        print("  " .. f.file)
      end
    end
  end

  return failed == 0
end

local function main(args)
  local cmd = args[1]

  if cmd == "report" then
    local output_dir = args[2] or "o/any"
    return report(output_dir) and 0 or 1
  end

  local source_file, output, luacheck_bin = args[1], args[2], args[3]
  if not source_file or not output or not luacheck_bin then
    io.stderr:write("usage: luacheck.lua <source_file> <output> <luacheck_bin>\n")
    io.stderr:write("       luacheck.lua report [output_dir]\n")
    return 1
  end

  check(source_file, output, luacheck_bin)
  return 0
end

os.exit(main({ ... }))
