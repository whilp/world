#!/usr/bin/env lua
-- teal ignore: type annotations needed

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")
local common = require("checker.common")

local supported_extensions = {
  [".lua"] = true,
}

local supported_patterns = {
  shebangs = {
    ["lua"] = true,
  },
  ignore = "ast%-grep%s+ignore%s*:?%s*(.*)",
}

local function parse_json_stream(stdout)
  local issues = {}
  for line in (stdout or ""):gmatch("[^\n]+") do
    local ok, obj = pcall(cosmo.DecodeJson, line)
    if ok and obj and obj.ruleId then
      table.insert(issues, {
        line = (obj.range and obj.range.start and obj.range.start.line or 0) + 1,
        column = (obj.range and obj.range.start and obj.range.start.column or 0) + 1,
        rule_id = obj.ruleId,
        message = obj.message,
        note = obj.note,
      })
    end
  end
  return issues
end

local function format_issues(issues, source)
  local lines = {}
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


local function main(source, out)
  if not source or not out then
    return 1, "usage: run-astgrep.lua <source> <out>"
  end

  unix.makedirs(path.dirname(out))

  local has_shebang, skip_reason = common.check_first_lines(source, supported_patterns)

  if not common.has_extension(source, supported_extensions) and not has_shebang then
    cosmo.Barf(out, common.format_output("ignore", "unsupported file type", "", ""))
    return 0
  end

  if skip_reason then
    cosmo.Barf(out, common.format_output("skip", skip_reason, "", ""))
    return 0
  end

  local sg = path.join(os.getenv("ASTGREP_BIN"), "sg")

  local handle = spawn({ sg, "scan", "--json=stream", source })
  local _, stdout, _ = handle:read()

  local issues = parse_json_stream(stdout)

  if #issues > 0 then
    local issue_text = format_issues(issues, source)
    cosmo.Barf(out, common.format_output("fail", #issues .. " issues", "", issue_text))
  else
    cosmo.Barf(out, common.format_output("pass", nil, "", ""))
  end

  return 0
end

if cosmo.is_main() then
  local code, err = main(...)
  if err then
    io.stderr:write(err .. "\n")
  end
  os.exit(code)
end
