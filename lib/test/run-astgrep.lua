#!/usr/bin/env lua

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

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

local function main(source, out)
  if not source or not out then
    return 1, "usage: run-astgrep.lua <source> <out.astgrep.checked>"
  end

  unix.makedirs(path.dirname(out))

  local sg = path.join(os.getenv("TEST_BIN"), "sg")

  local handle = spawn({ sg, "scan", "--json=stream", source })
  local _, stdout, exit_code = handle:read()

  local issues = parse_json_stream(stdout)

  if #issues > 0 then
    for _, issue in ipairs(issues) do
      io.stderr:write(string.format("%s:%d:%d: [%s] %s\n",
        source,
        issue.line,
        issue.column,
        issue.rule_id,
        issue.note or issue.message or ""))
    end
    cosmo.Barf(out, "fail\n")
    io.stderr:write("FAIL " .. source .. "\n")
    return 1
  end

  cosmo.Barf(out, "ok\n")
  io.stderr:write("PASS " .. source .. "\n")
  return 0
end

if cosmo.is_main() then
  local code, err = main(...)
  if err then
    io.stderr:write(err .. "\n")
  end
  os.exit(code)
end
