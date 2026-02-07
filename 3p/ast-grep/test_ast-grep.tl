#!/usr/bin/env run-test.lua

local cosmo = require("cosmo")
local spawn = require("cosmic.child").spawn
local path = require("cosmo.path")

global TEST_DIR: string
global TEST_TMPDIR: string

local bin = path.join(TEST_DIR, "bin", "sg")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "sg --version failed")
  local want = "ast-grep"
  assert((got as string):find(want, 1, true), "checking ast-grep, want: " .. want .. " got: " .. (got as string))
end
test_version()

local function test_rule_main_guard()
  local rule = ".ast-grep/rules/use-cosmo-is-main.yml"

  -- debug.getlocal pattern should trigger
  local bad1 = path.join(TEST_TMPDIR, "bad1.lua")
  cosmo.Barf(bad1, 'if not pcall(debug.getlocal, 4, 1) then main() end')
  local ok, out = spawn({ bin, "scan", "--rule", rule, bad1 }):read()
  assert(not ok, "rule should trigger on debug.getlocal pattern")
  assert((out as string):find("use%-cosmo%-is%-main"), "should report rule id")

  -- arg[0]:match pattern should trigger
  local bad2 = path.join(TEST_TMPDIR, "bad2.lua")
  cosmo.Barf(bad2, 'if arg[0]:match("main.lua$") then main() end')
  ok, out = spawn({ bin, "scan", "--rule", rule, bad2 }):read()
  assert(not ok, "rule should trigger on arg[0]:match pattern")

  -- good code should not trigger
  local good = path.join(TEST_TMPDIR, "good.lua")
  cosmo.Barf(good, 'if cosmo.is_main() then main() end')
  ok, out = spawn({ bin, "scan", "--rule", rule, good }):read()
  assert(ok, "rule should not trigger on good code: " .. tostring(out))
end
test_rule_main_guard()
