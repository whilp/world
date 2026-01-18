#!/usr/bin/env lua

local cosmo = require("cosmo")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")
local common = require("checker.common")

local record Issue
  line: integer
  column: integer
  rule_id: string
  message: string
  note: string
end

local record AstGrepRange
  start: AstGrepPosition
end

local record AstGrepPosition
  line: integer
  column: integer
end

local record AstGrepResult
  ruleId: string
  range: AstGrepRange
  message: string
  note: string
end

local supported_extensions: {string: boolean} = {
  [".lua"] = true,
  [".tl"] = true,
}

local record CheckPatterns
  shebangs: {string: boolean}
  ignore: string
end

local supported_patterns: CheckPatterns = {
  shebangs = {
    ["lua"] = true,
  },
  ignore = "ast%-grep%s+ignore%s*:?%s*(.*)",
}

local function parse_json_stream(stdout: string): {Issue}
  local issues: {Issue} = {}
  for line in (stdout or ""):gmatch("[^\n]+") do
    local ok, obj = pcall(cosmo.DecodeJson, line)
    if ok and obj then
      local result = obj as AstGrepResult
      if result.ruleId then
        local issue: Issue = {
          line = (result.range and result.range.start and result.range.start.line or 0) + 1,
          column = (result.range and result.range.start and result.range.start.column or 0) + 1,
          rule_id = result.ruleId,
          message = result.message,
          note = result.note,
        }
        table.insert(issues, issue)
      end
    end
  end
  return issues
end

local function format_issues(issues: {Issue}, source: string): string
  local lines: {string} = {}
  for _, issue in ipairs(issues) do
    table.insert(lines, string.format("%s:%d:%d: [%s] %s",
      source,
      issue.line,
      issue.column,
      issue.rule_id,
      issue.note or issue.message or ""))
  end
  return table.concat(lines, "\n")
end


local function main(source: string): integer, string
  if not source then
    return 1, "usage: run-astgrep.lua <source>"
  end

  local has_shebang, skip_reason = common.check_first_lines(source, supported_patterns)

  if not common.has_extension(source, supported_extensions) and not has_shebang then
    return common.write_result("ignore", "unsupported file type", "", "")
  end

  if skip_reason then
    return common.write_result("skip", skip_reason, "", "")
  end

  local sg = path.join(os.getenv("ASTGREP_BIN"), "sg")

  local handle = spawn({ sg, "scan", "--json=stream", source })
  local _, stdout, _ = handle:read()

  local issues = parse_json_stream(stdout as string)

  if #issues > 0 then
    local issue_text = format_issues(issues, source)
    return common.write_result("fail", #issues .. " issues", "", issue_text, source)
  end
  return common.write_result("pass", nil, "", "", source)
end

if cosmo.is_main() then
  local code, err = main(...)
  if err then
    io.stderr:write(err .. "\n")
  end
  os.exit(code)
end
