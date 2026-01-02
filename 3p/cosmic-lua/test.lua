-- test cosmic-lua bundled binary
-- run with: TEST_BIN_DIR=o/<platform>/cosmic-lua make o/any/3p/cosmic-lua/test.lua.luatest.ok

local lu = require("luaunit")
local spawn = require("cosmic.spawn")

local bin_dir = os.getenv("TEST_BIN_DIR")
if not bin_dir then
  print("TEST_BIN_DIR not set, skipping cosmic-lua binary tests")
  os.exit(0)
end

local cosmic_lua = bin_dir .. "/bin/cosmic-lua"

TestCosmicLuaBinary = {}

function TestCosmicLuaBinary:test_cosmic_spawn()
  local handle = spawn.spawn({cosmic_lua, "-e", "local s = require('cosmic.spawn'); print(s and 'ok' or 'fail')"})
  local ok, out = handle:read()
  lu.assertTrue(ok, "cosmic-lua exited with error")
  lu.assertStrContains(out, "ok")
end

function TestCosmicLuaBinary:test_cosmic_walk()
  local handle = spawn.spawn({cosmic_lua, "-e", "local w = require('cosmic.walk'); print(w and 'ok' or 'fail')"})
  local ok, out = handle:read()
  lu.assertTrue(ok, "cosmic-lua exited with error")
  lu.assertStrContains(out, "ok")
end

function TestCosmicLuaBinary:test_cosmic_help()
  local handle = spawn.spawn({cosmic_lua, "-e", "require('cosmic.help')"})
  local ok, out = handle:read()
  lu.assertTrue(ok, "cosmic-lua exited with error")
  lu.assertStrContains(out, "cosmic-lua")
  lu.assertStrContains(out, "cosmic.spawn")
end

function TestCosmicLuaBinary:test_luaunit_bundled()
  local handle = spawn.spawn({cosmic_lua, "-e", "local lu = require('luaunit'); print(lu and 'ok' or 'fail')"})
  local ok, out = handle:read()
  lu.assertTrue(ok, "cosmic-lua exited with error")
  lu.assertStrContains(out, "ok")
end

function TestCosmicLuaBinary:test_argparse_bundled()
  local handle = spawn.spawn({cosmic_lua, "-e", "local ap = require('argparse'); print(ap and 'ok' or 'fail')"})
  local ok, out = handle:read()
  lu.assertTrue(ok, "cosmic-lua exited with error")
  lu.assertStrContains(out, "ok")
end

function TestCosmicLuaBinary:test_lfs_bundled()
  local handle = spawn.spawn({cosmic_lua, "-e", "local lfs = require('lfs'); print(lfs and 'ok' or 'fail')"})
  local ok, out = handle:read()
  lu.assertTrue(ok, "cosmic-lua exited with error")
  lu.assertStrContains(out, "ok")
end

os.exit(lu.LuaUnit.run())
