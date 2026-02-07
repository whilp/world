#!/usr/bin/env lua

local cosmo = require("cosmo")
local path = require("cosmo.path")
local spawn = require("cosmic.child").spawn
local common = require("checker.common")

local record Issue
  line: integer
  column: integer
  severity: string
  message: string
end

local supported_extensions: {string: boolean} = {
  [".gs"] = true,
  [".js"] = true,
}

local function parse_output(stderr: string, source: string): {Issue}
  local issues: {Issue} = {}
  for line in (stderr or ""):gmatch("[^\n]+") do
    local ln, col = line:match(source:gsub("%-", "%%-") .. ":(%d+):(%d+)")
    if ln then
      local msg = stderr:match("SyntaxError: ([^\n]+)")
      table.insert(issues, {
        line = tonumber(ln) as integer,
        column = tonumber(col) as integer,
        severity = "error",
        message = msg or "syntax error",
      })
      break
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
    return 1, "usage: run-bun.lua <source>"
  end

  if not common.has_extension(source, supported_extensions) then
    return common.write_result("ignore", "unsupported file type", "", "", source)
  end

  local bun_bin = path.join(os.getenv("BUN_BIN") or "", "bin", "bun")

  local cmd: {string} = { bun_bin, "run", source }

  local handle = spawn(cmd)
  if handle.stdin then
    handle.stdin:close()
  end
  local stderr = handle.stderr and handle.stderr:read() or ""
  local exit_code = handle:wait()

  if exit_code ~= 0 then
    local issues = parse_output(stderr, source)
    if #issues > 0 then
      local issue_text = format_issues(issues, source)
      return common.write_result("fail", #issues .. " issues", "", issue_text, source)
    end
    return common.write_result("fail", "bun check failed", "", stderr, source)
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
