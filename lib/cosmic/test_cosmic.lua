local lu = require("luaunit")

TestCosmic = {}

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

function TestCosmic:test_lazy_loading()
  -- test that cosmic module lazy-loads submodules
  local cosmic = require("cosmic")
  local spawn = cosmic.spawn
  lu.assertNotNil(spawn)
  lu.assertNotNil(spawn.spawn)
end

function TestCosmic:test_spawn_basic()
  local spawn = require("cosmic.spawn")
  local handle = spawn({"echo", "hello"})
  lu.assertNotNil(handle)
  local ok, out = handle:read()
  lu.assertTrue(ok)
  lu.assertEquals(out:gsub("%s+$", ""), "hello")
end

function TestCosmic:test_walk_basic()
  local walk = require("cosmic.walk")
  local files = walk.collect(".", "^test_cosmic%.lua$")
  lu.assertNotNil(files)
  lu.assertTrue(#files >= 1)
end
