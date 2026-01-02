#!/usr/bin/env lua

local unix = require("cosmo.unix")
local path = require("cosmo.path")
local cosmo = require("cosmo")
local spawn = require("cosmic.spawn").spawn
local walk = require("cosmic.walk")

local function parse_json_stream(stdout)
  local issues = {}
  for line in (stdout or ""):gmatch("[^\n]+") do
    local ok, obj = pcall(cosmo.DecodeJson, line)
    if ok and obj and obj.ruleId then
      table.insert(issues, {
        line = (obj.range and obj.range.start and obj.range.start.line or 0) + 1,
        column = (obj.range and obj.range.start and obj.range.start.column or 0) + 1,
        end_line = (obj.range and obj.range["end"] and obj.range["end"].line or 0) + 1,
        end_column = (obj.range and obj.range["end"] and obj.range["end"].column or 0) + 1,
        rule_id = obj.ruleId,
        severity = obj.severity,
        message = obj.message,
        note = obj.note,
        text = obj.text,
      })
    end
  end
  return issues
end

local function check(source_file, output, ast_grep_bin)
  local output_dir = path.dirname(output)
  unix.makedirs(output_dir)

  print("# ast-grep " .. source_file)

  local cmd = {
    ast_grep_bin,
    "scan",
    "--json=stream",
    source_file,
  }

  local handle = spawn(cmd)

  local _, stdout, exit_code = handle:read()

  local issues = parse_json_stream(stdout)

  if #issues > 0 then
    for _, issue in ipairs(issues) do
      io.write(string.format("%s:%d:%d: [%s] %s\n",
        source_file,
        issue.line,
        issue.column,
        issue.rule_id,
        issue.note or issue.message:match("^[^\n]+")))
    end
  end

  local passed = (exit_code == 0)

  local result = {
    file = source_file,
    checker = "ast-grep",
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
  local by_rule = {}

  for _, filepath in ipairs(walk.collect(output_dir, "%.ok$")) do
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
          by_rule[issue.rule_id] = (by_rule[issue.rule_id] or 0) + 1
        end
      end
    end
  end

  local total = passed + failed

  print("ast-grep report")
  print("───────────────────────────────")
  print(string.format("  files checked:  %d", total))
  print(string.format("  passed:         %d", passed))
  print(string.format("  failed:         %d", failed))
  print(string.format("  total issues:   %d", total_issues))
  print("")

  if total_issues > 0 then
    print("issues by rule:")
    local rules = {}
    for rule in pairs(by_rule) do table.insert(rules, rule) end
    table.sort(rules, function(a, b) return by_rule[b] < by_rule[a] end)
    for _, rule in ipairs(rules) do
      print(string.format("  %s: %d", rule, by_rule[rule]))
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

  local source_file, output, ast_grep_bin = args[1], args[2], args[3]
  if not source_file or not output or not ast_grep_bin then
    io.stderr:write("usage: ast-grep.lua <source_file> <output> <ast_grep_bin>\n")
    io.stderr:write("       ast-grep.lua report [output_dir]\n")
    return 1
  end

  local passed = check(source_file, output, ast_grep_bin)
  return passed and 0 or 1
end

if ... then
  os.exit(main({ ... }))
else
  return {
    parse_json_stream = parse_json_stream,
    check = check,
    report = report,
    main = main,
  }
end
