#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

global TEST_DIR: string
global TEST_TMPDIR: string

-- Test that the teal library is bundled with nvim
local function test_teal_library_exists()
  local tl_lib = path.join(TEST_DIR, "share", "nvim", "runtime", "lua", "tl.lua")
  assert(unix.stat(tl_lib), "tl.lua should exist at: " .. tl_lib)
  print("✓ teal library is bundled")
end
test_teal_library_exists()

-- Test that nvim can load teal via require("tl").loader()
local function test_nvim_can_load_teal()
  local nvim = path.join(TEST_DIR, "bin", "nvim")

  -- Write test script that loads teal and writes result to file
  local test_script = path.join(TEST_TMPDIR, "test.lua")
  local f = io.open(test_script, "w")
  assert(f, "failed to create test script")

  local result_file = path.join(TEST_TMPDIR, "teal-test-result.txt")
  f:write([[
local tl = require("tl")
tl.loader()

-- Write result to file
local f = io.open("]] .. result_file .. [[", "w")
if f then
  f:write("TEAL_LOADER_LOADED\n")
  f:close()
end
]])
  f:close()

  -- Run nvim with the test script
  local ok = spawn({ nvim, "--headless", "-l", test_script, "+qa" }):wait()
  assert(ok, "nvim failed to run test script")

  -- Check result file
  local result_f = io.open(result_file, "r")
  assert(result_f, "result file not created - teal loader may have failed to load")
  local content = result_f:read("*a")
  result_f:close()

  assert(content:find("TEAL_LOADER_LOADED", 1, true),
    "expected teal loader to load, got: " .. content)

  print("✓ nvim can load and initialize teal loader")
end
test_nvim_can_load_teal()
