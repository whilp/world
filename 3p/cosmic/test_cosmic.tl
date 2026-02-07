#!/usr/bin/env run-test.lua
local unix = require("cosmo.unix")
local path = require("cosmo.path")

-- Note: TEST_DEPS tests removed - TEST_DEPS is a Make variable not passed to Lua tests

local function test_env_vars()
  assert(os.getenv("TEST_O") ~= nil, "TEST_O should be set")
  assert(os.getenv("TEST_PLATFORM") ~= nil, "TEST_PLATFORM should be set")
end
test_env_vars()

local function test_require_cosmic()
  local cosmic = require("cosmic")
  assert(cosmic ~= nil, "cosmic should not be nil")
  assert(cosmic._VERSION == "0.1.0", "expected version 0.1.0, got " .. tostring(cosmic._VERSION))
end
test_require_cosmic()

local function test_require_cosmic_child()
  local child_mod = require("cosmic.child")
  assert(child_mod ~= nil, "child_mod should not be nil")
  assert(child_mod.spawn ~= nil, "child_mod.spawn should not be nil")
end
test_require_cosmic_child()

local function test_require_cosmic_fs()
  local fs = require("cosmic.fs")
  assert(fs ~= nil, "fs should not be nil")
  assert(fs.walk ~= nil, "fs.walk should not be nil")
  assert(fs.collect ~= nil, "fs.collect should not be nil")
  assert(fs.collect_all ~= nil, "fs.collect_all should not be nil")
  assert(fs.files ~= nil, "fs.files should not be nil")
end
test_require_cosmic_fs()

local function test_cosmic_main_exists()
  local cosmic = require("cosmic")
  assert(cosmic.main ~= nil, "cosmic.main should not be nil")
  assert(type(cosmic.main) == "function", "expected cosmic.main to be function, got " .. type(cosmic.main))
end
test_cosmic_main_exists()

local function test_cosmic_main_returns_when_not_main()
  local cosmic = require("cosmic")
  local called = false
  local function app(): number
    called = true
    return 0
  end
  -- when required (not main), cosmic.main should return without calling fn
  cosmic.main(app as function({string}, any): number, string)
  assert(not called, "expected app not to be called when not main")
end
test_cosmic_main_returns_when_not_main()
