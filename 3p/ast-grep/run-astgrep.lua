#!/usr/bin/env lua

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
    local reason = line:match("ast%-grep%s+ignore%s*(.*)")
    if reason then
      f:close()
      return has_shebang, reason ~= "" and reason or "directive"
    end
  end
  f:close()
  return has_shebang, nil
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

  local has_shebang, skip_reason = check_first_lines(source)

  if not has_supported_extension(source) and not has_shebang then
    cosmo.Barf(out, "ignore: unsupported file type\n")
    return 0
  end

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
