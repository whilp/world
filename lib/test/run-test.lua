#!/usr/bin/env lua

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local function main(test, out)
  if not test or not out then
    return 1, "usage: run-test.lua <test> <out.ok>"
  end

  unix.makedirs(path.dirname(out))

  TEST_TMPDIR = unix.mkdtemp("/tmp/test_XXXXXX")
  TEST_DIR = os.getenv("TEST_DIR")

  local ok, err = pcall(dofile, test)
  unix.rmrf(TEST_TMPDIR)

  if not ok then
    local msg = tostring(err)
    -- check for SKIP prefix in assertion message
    local skip_reason = msg:match("SKIP%s+(.+)")
    if skip_reason then
      cosmo.Barf(out, "skip: " .. skip_reason .. "\n")
      io.stderr:write("SKIP " .. test .. " (" .. skip_reason .. ")\n")
      return 0
    end
    -- strip path prefix to show just filename:line: message
    local short = msg:gsub("^.-/([^/]+:%d+:)", "%1")
    return 1, "FAIL " .. test .. "\n     " .. short
  end

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
