#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

global TEST_DIR: string

local bin = path.join(TEST_DIR, "bun")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "bun --version failed")
  assert((got as string):find("1%.", 1), "expected version starting with 1.")
end
test_version()

local function test_eval()
  local ok, got = spawn({ bin, "-e", "console.log('hello')" }):read()
  assert(ok, "bun -e failed")
  assert((got as string):find("hello", 1, true), "expected hello output")
end
test_eval()
