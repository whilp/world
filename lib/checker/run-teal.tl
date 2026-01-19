#!/usr/bin/env lua
-- Teal type checker using cosmic --check

local cosmo = require("cosmo")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")
local common = require("checker.common")

local record Issue
  line: integer
  column: integer
  severity: string
  message: string
end

local record CheckPatterns
  shebangs: {string: boolean}
  ignore: string
end

local supported_extensions: {string: boolean} = {
  [".lua"] = true,
  [".tl"] = true,
}

local supported_patterns: CheckPatterns = {
  shebangs = {
    ["lua"] = true,
  },
  ignore = "teal%s+ignore%s*:?%s*(.*)",
}

local function parse_output(output: string): {Issue}
  local issues: {Issue} = {}
  for line in (output or ""):gmatch("[^\n]+") do
    -- Format: filename:line:column: severity: message
    local ln, col, severity, msg = line:match(":(%d+):(%d+): (%w+): (.+)$")
    if ln then
      table.insert(issues, {
        line = tonumber(ln) as integer,
        column = tonumber(col) as integer,
        severity = severity,
        message = msg,
      })
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
      issue.severity,
      issue.message or ""))
  end
  return table.concat(lines, "\n")
end

local function main(source: string): integer, string
  if not source then
    return 1, "usage: run-teal.lua <source>"
  end

  local has_shebang, skip_reason = common.check_first_lines(source, supported_patterns)

  if not common.has_extension(source, supported_extensions) and not has_shebang then
    return common.write_result("ignore", "unsupported file type", "", "")
  end

  if skip_reason then
    return common.write_result("skip", skip_reason, "", "")
  end

  local cosmic_bin = os.getenv("COSMIC_BIN") or "cosmic"
  local cmd: {string} = { cosmic_bin, "--check", source }

  local handle = spawn(cmd)
  if handle.stdin then
    handle.stdin:close()
  end
  local stdout = handle.stdout and handle.stdout:read() or ""
  local stderr = handle.stderr and handle.stderr:read() or ""
  local exit_code = handle:wait()

  -- cosmic --check outputs to stderr for errors/warnings, stdout for success message
  local issues = parse_output(stderr)

  if #issues > 0 or exit_code ~= 0 then
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
