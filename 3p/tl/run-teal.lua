#!/usr/bin/env lua
-- teal ignore: type annotations needed

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local supported_extensions = {
  [".lua"] = true,
}

local supported_shebangs = {
  ["lua"] = true,
}

local function has_supported_extension(file)
  for ext in pairs(supported_extensions) do
    if file:sub(-#ext) == ext then
      return true
    end
  end
  return false
end

local function check_first_lines(file)
  local f = io.open(file, "r")
  if not f then
    return nil, nil
  end
  local has_shebang = false
  for i = 1, 10 do
    local line = f:read("*l")
    if not line then
      break
    end
    if i == 1 then
      local interp = line:match("^#!.-/([%w_-]+)%s*$") or line:match("^#!/usr/bin/env%s+([%w_-]+)")
      if interp and supported_shebangs[interp] then
        has_shebang = true
      end
    end
    local reason = line:match("teal%s+ignore%s*:?%s*(.*)")
    if reason then
      f:close()
      return has_shebang, reason ~= "" and reason or "directive"
    end
  end
  f:close()
  return has_shebang, nil
end

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

local function format_output(status, message, stdout, stderr)
  local lines = {}
  if message and message ~= "" then
    table.insert(lines, status .. ": " .. message)
  else
    table.insert(lines, status)
  end
  table.insert(lines, "")
  table.insert(lines, "## stdout")
  table.insert(lines, "")
  table.insert(lines, stdout or "")
  table.insert(lines, "## stderr")
  table.insert(lines, "")
  table.insert(lines, stderr or "")
  return table.concat(lines, "\n")
end

local function main(source, out)
  if not source or not out then
    return 1, "usage: run-teal.lua <source> <out>"
  end

  unix.makedirs(path.dirname(out))

  local has_shebang, skip_reason = check_first_lines(source)

  if not has_supported_extension(source) and not has_shebang then
    cosmo.Barf(out, format_output("ignore", "unsupported file type", "", ""))
    return 0
  end

  if skip_reason then
    cosmo.Barf(out, format_output("skip", skip_reason, "", ""))
    return 0
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
    cosmo.Barf(out, format_output("fail", #issues .. " issues", "", issue_text))
  else
    cosmo.Barf(out, format_output("pass", nil, "", ""))
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
