local lu = require("luaunit")
local spawn = require("cosmic.spawn").spawn
local path = require("cosmo.path")

local bin_dir = path.join(os.getenv("TEST_BIN_DIR"), "bin")

TestCosmos = {}

function TestCosmos:test_lua()
  local handle = spawn({ path.join(bin_dir, "lua"), "-v" })
  lu.assertEquals(handle:wait(), 0)
end

function TestCosmos:test_zip()
  local handle = spawn({ path.join(bin_dir, "zip"), "--version" })
  lu.assertEquals(handle:wait(), 0)
end

function TestCosmos:test_unzip()
  local handle = spawn({ path.join(bin_dir, "unzip"), "-v" })
  lu.assertEquals(handle:wait(), 0)
end

function TestCosmos:test_make()
  local handle = spawn({ path.join(bin_dir, "make"), "-v" })
  lu.assertEquals(handle:wait(), 0)
end
