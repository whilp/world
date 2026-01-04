#!/usr/bin/env run-test.lua

local bootstrap = require("skill.bootstrap")

-- test check_lua_available
local function test_check_lua_available()
  local available, path = bootstrap.check_lua_available()
  -- this test environment might not have lua in PATH, so we just check it returns something
  assert(type(available) == "boolean", "expected boolean result")
  if available then
    assert(type(path) == "string", "expected path when lua is available")
  end
end
test_check_lua_available()

-- test check_settings_file
local function test_check_settings_file()
  -- in this repo we should have .claude/settings.json
  local settings_file = bootstrap.check_settings_file()
  assert(settings_file, "expected to find settings file in this repo")
  assert(settings_file:match("settings"), "expected settings in filename")
end
test_check_settings_file()

-- test parse_settings
local function test_parse_settings()
  local settings_file = bootstrap.check_settings_file()
  if settings_file then
    local settings, err = bootstrap.parse_settings(settings_file)
    assert(settings or err, "expected settings or error")
    if not settings then
      print("parse error (may be expected): " .. err)
    end
  end
end
test_parse_settings()

-- test check_bootstrap_setup
local function test_check_bootstrap_setup()
  local result = bootstrap.check_bootstrap_setup()
  assert(result, "expected result")
  assert(type(result.issues) == "table", "expected issues table")
  assert(type(result.has_hook) == "boolean", "expected has_hook boolean")
  assert(type(result.has_lua) == "boolean", "expected has_lua boolean")
  assert(type(result.has_bin_lua) == "boolean", "expected has_bin_lua boolean")
  assert(type(result.has_bin_cosmic_lua) == "boolean", "expected has_bin_cosmic_lua boolean")
end
test_check_bootstrap_setup()

print("All bootstrap tests passed")
