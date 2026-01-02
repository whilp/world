#!/usr/bin/env lua
-- Debug: print LUA_PATH
io.stderr:write("DEBUG luacheck.lua: LUA_PATH=" .. (os.getenv("LUA_PATH") or "nil") .. "\n")
io.stderr:write("DEBUG luacheck.lua: package.path=" .. package.path .. "\n")

local unix = require("cosmo.unix")
local path = require("cosmo.path")
local cosmo = require("cosmo")

io.stderr:write("DEBUG luacheck.lua: about to require spawn\n")
local spawn = require("spawn").spawn
io.stderr:write("DEBUG luacheck.lua: spawn loaded successfully\n")

local walk = require("walk")
io.stderr:write("DEBUG luacheck.lua: walk loaded successfully\n")

local function parse_plain(stdout)
  local issues = {}
  for line in (stdout or ""):gmatch("[^\n]+") do
    -- plain format: file:line:col-endcol: (CODE) message
    local _, ln, col, endcol, code, msg = line:match("^(.+):(%d+):(%d+)-(%d+): %(([WE]%d+)%) (.+)$")
    if not ln then
      -- without ranges: file:line:col: (CODE) message
      _, ln, col, code, msg = line:match("^(.+):(%d+):(%d+): %(([WE]%d+)%) (.+)$")
    end
    if ln then
      table.insert(issues, {
        line = tonumber(ln),
        column = tonumber(col),
        end_column = endcol and tonumber(endcol),
        code = code,
        message = msg,
      })
    end
  end
  return issues
end

local function check(source_file, output, luacheck_bin)
  local output_dir = path.dirname(output)
  unix.makedirs(output_dir)

  print("# luacheck " .. source_file)

  local source = cosmo.Slurp(source_file)
  local handle = spawn({
    luacheck_bin,
    "--formatter", "plain",
    "--codes",
    "--ranges",
    "--filename", source_file,
    "-",
  }, { stdin = source })

  local _, stdout, exit_code = handle:read()

  if stdout and #stdout > 0 then
    io.write(stdout)
  end

  local issues = parse_plain(stdout)
  local passed = (exit_code == 0)

  local result = {
    file = source_file,
    checker = "luacheck",
    passed = passed,
    exit_code = exit_code,
    issues = issues,
  }

  cosmo.Barf(output, "return " .. cosmo.EncodeLua(result) .. "\n")

  return passed
end

local function report(output_dir)
  local files = {}
  local passed = 0
  local failed = 0
  local total_issues = 0
  local by_code = {}

  for _, filepath in ipairs(walk.collect(output_dir, "%.luacheck%.ok$")) do
    local chunk = loadfile(filepath)
    if chunk then
      local result = chunk()
      if result then
        table.insert(files, result)
        if result.passed then
          passed = passed + 1
        else
          failed = failed + 1
        end
        for _, issue in ipairs(result.issues or {}) do
          total_issues = total_issues + 1
          by_code[issue.code] = (by_code[issue.code] or 0) + 1
        end
      end
    end
  end

  local total = passed + failed

  print("luacheck report")
  print("───────────────────────────────")
  print(string.format("  files checked:  %d", total))
  print(string.format("  passed:         %d", passed))
  print(string.format("  failed:         %d", failed))
  print(string.format("  total issues:   %d", total_issues))
  print("")

  if total_issues > 0 then
    print("issues by code:")
    local codes = {}
    for code in pairs(by_code) do table.insert(codes, code) end
    table.sort(codes, function(a, b) return by_code[b] < by_code[a] end)
    for _, code in ipairs(codes) do
      print(string.format("  %s: %d", code, by_code[code]))
    end
    print("")
  end

  if failed > 0 then
    print("files with issues:")
    table.sort(files, function(a, b) return a.file < b.file end)
    for _, f in ipairs(files) do
      if not f.passed then
        print(string.format("  %s (%d)", f.file, #(f.issues or {})))
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

  local passed = check(source_file, output, luacheck_bin)
  return passed and 0 or 1
end

if ... then
  os.exit(main({ ... }))
else
  return {
    parse_plain = parse_plain,
    check = check,
    report = report,
    main = main,
  }
end
