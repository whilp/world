#!/usr/bin/env lua

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local supported_extensions = {
  [".lua"] = true,
}

local function is_supported(file)
  for ext in pairs(supported_extensions) do
    if file:sub(-#ext) == ext then
      return true
    end
  end
  return false
end

local function check_skip_directive(file)
  local f = io.open(file, "r")
  if not f then
    return nil
  end
  for i = 1, 10 do
    local line = f:read("*l")
    if not line then
      break
    end
    local reason = line:match("ast%-grep%s+ignore%s*(.*)")
    if reason then
      f:close()
      return reason ~= "" and reason or "directive"
    end
  end
  f:close()
  return nil
end

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

  if not is_supported(source) then
    cosmo.Barf(out, "ignore: unsupported file type\n")
    return 0
  end

  local skip_reason = check_skip_directive(source)
  if skip_reason then
    cosmo.Barf(out, "skip: " .. skip_reason .. "\n")
    io.stderr:write("SKIP " .. source .. " (" .. skip_reason .. ")\n")
    return 0
  end

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
