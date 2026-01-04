#!/usr/bin/env run-test.lua
-- teal ignore: test file

local cosmo = require("cosmo")
local spawn = require("cosmic.spawn")
local path = require("cosmo.path")

local bin = path.join(TEST_DIR, "sg")

local function test_version()
  local ok, got = spawn({ bin, "--version" }):read()
  assert(ok, "sg --version failed")
  local want = "ast-grep"
  assert(got:find(want, 1, true), "checking ast-grep, want: " .. want .. " got: " .. got)
end
test_version()

local function test_rule_debug_getlocal()
  local rule = ".ast-grep/rules/avoid-debug-getlocal-main-guard.yml"

  -- bad code should trigger
  local bad = path.join(TEST_TMPDIR, "bad.lua")
  cosmo.Barf(bad, 'if not pcall(debug.getlocal, 4, 1) then main() end')
  local ok, out = spawn({ bin, "scan", "--rule", rule, bad }):read()
  assert(not ok, "rule should trigger on bad code")
  assert(out:find("avoid%-debug%-getlocal%-main%-guard"), "should report rule id")

  -- good code should not trigger
  local good = path.join(TEST_TMPDIR, "good.lua")
  cosmo.Barf(good, 'if cosmo.is_main() then main() end')
  ok, out = spawn({ bin, "scan", "--rule", rule, good }):read()
  assert(ok, "rule should not trigger on good code: " .. tostring(out))
end
test_rule_debug_getlocal()
