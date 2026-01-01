#!/usr/bin/env lua
local args = { ... }

local unix = require("cosmo.unix")
local path = require("cosmo.path")
local cosmo = require("cosmo")
local spawn = require("spawn").spawn

local function report(output_dir)
  local files = {}
  local passed = 0
  local failed = 0

  -- find all .luacheck.ok files
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

-- report mode
if args[1] == "report" then
  local output_dir = args[2] or "o/any"
  local ok = report(output_dir)
  os.exit(ok and 0 or 1)
end

-- check mode
local source_file, output, luacheck_bin = args[1], args[2], args[3]

if not source_file or not output or not luacheck_bin then
  io.stderr:write("usage: luacheck.lua <source_file> <output> <luacheck_bin>\n")
  io.stderr:write("       luacheck.lua report [output_dir]\n")
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

-- TODO: fail build once all files pass
-- don't fail the build; results are recorded in .ok file
