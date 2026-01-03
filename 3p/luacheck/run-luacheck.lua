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
  ignore = "luacheck%s+ignore%s*:?%s*(.*)",
}

local function parse_plain(stdout)
  local issues = {}
  for line in (stdout or ""):gmatch("[^\n]+") do
    local _, ln, col, endcol, code, msg = line:match("^(.+):(%d+):(%d+)-(%d+): %(([WE]%d+)%) (.+)$")
    if not ln then
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

local function format_issues(issues, source)
  local lines = {}
  for _, issue in ipairs(issues) do
    table.insert(lines, string.format("%s:%d:%d: (%s) %s",
      source,
      issue.line,
      issue.column,
      issue.code,
      issue.message or ""))
  end
  return table.concat(lines, "\n")
end


local function main(source, out)
  if not source or not out then
    return 1, "usage: run-luacheck.lua <source> <out>"
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

  local luacheck_bin = path.join(os.getenv("LUACHECK_BIN"), "luacheck")

  local source_content = cosmo.Slurp(source)
  local handle = spawn({
    luacheck_bin,
    "--formatter", "plain",
    "--codes",
    "--ranges",
    "--filename", source,
    "-",
  }, { stdin = source_content })

  local _, stdout, _ = handle:read()

  local issues = parse_plain(stdout)

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
