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
  ignore = "teal%s+ignore%s*:?%s*(.*)",
}

local function parse_output(stderr)
  local issues = {}
  local current_severity = nil

  for line in (stderr or ""):gmatch("[^\n]+") do
    if line:match("^%d+ warnings?:$") then
      current_severity = "warning"
    elseif line:match("^%d+ errors?:$") then
      current_severity = "error"
    elseif line:match("^=+$") or line:match("^%-+$") then
      -- separator lines, skip
    elseif current_severity then
      local file, ln, col, msg = line:match("^(.+):(%d+):(%d+): (.+)$")
      if ln then
        table.insert(issues, {
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

local function format_issues(issues, source)
  local lines = {}
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


local function main(source)
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

  local tl_bin = path.join(os.getenv("TL_BIN"), "tl")

  local handle = spawn({ tl_bin, "check", source })
  if handle.stdin then
    handle.stdin:close()
  end
  local stderr = handle.stderr and handle.stderr:read() or ""
  local exit_code = handle:wait()

  local issues = parse_output(stderr)

  if #issues > 0 then
    local issue_text = format_issues(issues, source)
    return common.write_result("fail", #issues .. " issues", "", issue_text)
  end
  return common.write_result("pass", nil, "", "")
end

if cosmo.is_main() then
  local code, err = main(...)
  if err then
    io.stderr:write(err .. "\n")
  end
  os.exit(code)
end
