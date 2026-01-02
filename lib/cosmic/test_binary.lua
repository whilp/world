-- test cosmic bundled binary

local lu = require("luaunit")
local spawn = require("cosmic.spawn")

local bin_dir = os.getenv("TEST_BIN_DIR")
local cosmic = bin_dir and (bin_dir .. "/bin/cosmic")

TestCosmicBinary = {}

function TestCosmicBinary:test_cosmic_spawn()
  if not cosmic then lu.skip("TEST_BIN_DIR not set") return end
  local handle = spawn({cosmic, "-e", "local s = require('cosmic.spawn'); print(s and 'ok' or 'fail')"})
  local ok, out = handle:read()
  lu.assertTrue(ok, "cosmic exited with error")
  lu.assertStrContains(out, "ok")
end

function TestCosmicBinary:test_cosmic_walk()
  if not cosmic then lu.skip("TEST_BIN_DIR not set") return end
  local handle = spawn({cosmic, "-e", "local w = require('cosmic.walk'); print(w and 'ok' or 'fail')"})
  local ok, out = handle:read()
  lu.assertTrue(ok, "cosmic exited with error")
  lu.assertStrContains(out, "ok")
end

function TestCosmicBinary:test_cosmic_help()
  if not cosmic then lu.skip("TEST_BIN_DIR not set") return end
  local handle = spawn({cosmic, "-e", "require('cosmic.help')"})
  local ok, out = handle:read()
  lu.assertTrue(ok, "cosmic exited with error")
  lu.assertStrContains(out, "cosmic")
  lu.assertStrContains(out, "cosmic.spawn")
end

function TestCosmicBinary:test_luaunit_bundled()
  if not cosmic then lu.skip("TEST_BIN_DIR not set") return end
  local handle = spawn({cosmic, "-e", "local lu = require('luaunit'); print(lu and 'ok' or 'fail')"})
  local ok, out = handle:read()
  lu.assertTrue(ok, "cosmic exited with error")
  lu.assertStrContains(out, "ok")
end

function TestCosmicBinary:test_argparse_bundled()
  if not cosmic then lu.skip("TEST_BIN_DIR not set") return end
  local handle = spawn({cosmic, "-e", "local ap = require('argparse'); print(ap and 'ok' or 'fail')"})
  local ok, out = handle:read()
  lu.assertTrue(ok, "cosmic exited with error")
  lu.assertStrContains(out, "ok")
end

function TestCosmicBinary:test_lfs_bundled()
  if not cosmic then lu.skip("TEST_BIN_DIR not set") return end
  local handle = spawn({cosmic, "-e", "local lfs = require('lfs'); print(lfs and 'ok' or 'fail')"})
  local ok, out = handle:read()
  lu.assertTrue(ok, "cosmic exited with error")
  lu.assertStrContains(out, "ok")
end
