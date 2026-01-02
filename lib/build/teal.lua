#!/usr/bin/env lua
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local cosmo = require("cosmo")
local spawn = require("spawn").spawn
local walk = require("walk")

local function parse_output(stdout)
  local issues = {}
  local current_severity = nil

  for line in (stdout or ""):gmatch("[^\n]+") do
    if line:match("^%d+ warnings?:$") then
      current_severity = "warning"
    elseif line:match("^%d+ errors?:$") then
      current_severity = "error"
    elseif current_severity and not (line:match("^=+$") or line:match("^%-+$")) then
      local file, ln, col, msg = line:match("^(.+):(%d+):(%d+): (.+)$")
      if ln then
        table.insert(issues, {
          file = file,
          line = tonumber(ln),
          column = tonumber(col),
          severity = current_severity,
          message = msg,
        })
      end
    end
  end
  return issues
end

local function check(source_file, output, tl_bin, lua_dist)
  local output_dir = path.dirname(output)
  unix.makedirs(output_dir)

  print("# teal " .. source_file)

  local handle = spawn({
    lua_dist,
    tl_bin,
    "check",
    source_file,
  })

  if handle.stdin then handle.stdin:close() end
  local stderr_output = handle.stderr:read()
  local exit_code = handle:wait()

  if stderr_output and #stderr_output > 0 then
    io.write(stderr_output)
  end

  local issues = parse_output(stderr_output)
  local passed = (exit_code == 0)

  local result = {
    file = source_file,
    checker = "teal",
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
  local by_severity = {}

  for _, filepath in ipairs(walk.collect(output_dir, "%.teal%.ok$")) do
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
          by_severity[issue.severity] = (by_severity[issue.severity] or 0) + 1
        end
      end
    end
  end

  local total = passed + failed

  print("teal report")
  print("───────────────────────────────")
  print(string.format("  files checked:  %d", total))
  print(string.format("  passed:         %d", passed))
  print(string.format("  failed:         %d", failed))
  print(string.format("  total issues:   %d", total_issues))
  print("")

  if total_issues > 0 then
    print("issues by severity:")
    local severities = {}
    for severity in pairs(by_severity) do table.insert(severities, severity) end
    table.sort(severities, function(a, b) return by_severity[b] < by_severity[a] end)
    for _, severity in ipairs(severities) do
      print(string.format("  %s: %d", severity, by_severity[severity]))
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

  local source_file, output, tl_bin, lua_dist = args[1], args[2], args[3], args[4]
  if not source_file or not output or not tl_bin or not lua_dist then
    io.stderr:write("usage: teal.lua <source_file> <output> <tl_bin> <lua_dist>\n")
    io.stderr:write("       teal.lua report [output_dir]\n")
    return 1
  end

  local passed = check(source_file, output, tl_bin, lua_dist)
  return passed and 0 or 1
end

if ... then
  os.exit(main({ ... }))
else
  return {
    parse_output = parse_output,
    check = check,
    report = report,
    main = main,
  }
end
