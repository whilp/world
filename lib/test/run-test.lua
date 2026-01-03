#!/usr/bin/env lua

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local function main(test, out, ...)
  if not test or not out then
    return 1, "usage: run-test.lua <test> <out.ok> [deps...]"
  end

  unix.makedirs(path.dirname(out))

  TEST_TMPDIR = unix.mkdtemp("/tmp/test_XXXXXX")
  TEST_DEPS = {...}

  local ok, err = pcall(dofile, test)
  if not ok then
    unix.rmrf(TEST_TMPDIR)
    local msg = tostring(err)
    -- strip path prefix to show just filename:line: message
    local short = msg:gsub("^.-/([^/]+:%d+:)", "%1")
    return 1, "FAIL " .. test .. "\n     " .. short
  end

  unix.rmrf(TEST_TMPDIR)

  cosmo.Barf(out, "ok\n")
  io.stderr:write("PASS " .. test .. "\n")

  return 0
end

if cosmo.is_main() then
  local code, err = main(...)
  if err then
    io.stderr:write(err .. "\n")
  end
  os.exit(code)
end
