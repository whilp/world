local lu = require("luaunit")
local spawn = require("spawn").spawn

return function(bin_dir)
  local bin = bin_dir .. "/bin/nvim"

  TestNvim = {}

  function TestNvim:test_version()
    local handle = spawn({bin, "--version"})
    lu.assertEquals(handle:wait(), 0)
  end

  return TestNvim
end
