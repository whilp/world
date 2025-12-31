local lu = require("luaunit")
local spawn = require("spawn")
local path = require("cosmo.path")

return function(env)
  local bin = path.join(env.BIN_DIR, "bin", "nvim")

  TestNvim = {}

  function TestNvim:test_version()
    local handle = spawn({ bin, "--version" })
    lu.assertEquals(handle:wait(), 0)
  end

  return TestNvim
end
