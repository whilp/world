#!/usr/bin/env run-test.lua
-- teal ignore: test file
local lu = require("luaunit")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

TestCosmic = {}

function TestCosmic:test_deps_passed()
  lu.assertNotNil(TEST_DEPS)
  lu.assertTrue(#TEST_DEPS > 0, "expected deps to be passed")
end

function TestCosmic:test_env_vars()
  lu.assertNotNil(os.getenv("TEST_O"))
  lu.assertNotNil(os.getenv("TEST_PLATFORM"))
end

function TestCosmic:test_lib_files_exist()
  local test_o = os.getenv("TEST_O")
  for _, dep in ipairs(TEST_DEPS) do
    if dep:match("%.lua$") then
      local stat = unix.stat(dep)
      lu.assertNotNil(stat, "expected " .. dep .. " to exist")
    end
  end
end

function TestCosmic:test_require_cosmic()
  local cosmic = require("cosmic")
  lu.assertNotNil(cosmic)
  lu.assertEquals(cosmic._VERSION, "0.1.0")
end

function TestCosmic:test_require_cosmic_spawn()
  local spawn = require("cosmic.spawn")
  lu.assertNotNil(spawn)
  lu.assertNotNil(spawn.spawn)
end

function TestCosmic:test_require_cosmic_walk()
  local walk = require("cosmic.walk")
  lu.assertNotNil(walk)
  lu.assertNotNil(walk.walk)
  lu.assertNotNil(walk.collect)
  lu.assertNotNil(walk.collect_all)
end

function TestCosmic:test_require_cosmic_help()
  -- help module prints on load, but should still return a table
  local help = require("cosmic.help")
  lu.assertNotNil(help)
  lu.assertNotNil(help.modules)
  lu.assertNotNil(help.print_help)
end

function TestCosmic:test_cosmic_main_exists()
  local cosmic = require("cosmic")
  lu.assertNotNil(cosmic.main)
  lu.assertEquals(type(cosmic.main), "function")
end

function TestCosmic:test_cosmic_main_returns_when_not_main()
  local cosmic = require("cosmic")
  local called = false
  local function app()
    called = true
    return 0
  end
  -- when required (not main), cosmic.main should return without calling fn
  cosmic.main(app)
  lu.assertFalse(called)
end
