local lu = require("luaunit")
local spawn = require("spawn")
local path = require("cosmo.path")

local bin = path.join(ENV.BIN_DIR, "bin", "nvim")

TestNvim = {}

function TestNvim:test_version()
  local handle = spawn({ bin, "--version" })
  lu.assertEquals(handle:wait(), 0)
end
