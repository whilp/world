local lu = require("luaunit")
local spawn = require("spawn")
local path = require("cosmo.path")

local bin = path.join(os.getenv("TEST_BIN_DIR"), "bin", "comrak")

TestComrak = {}

function TestComrak:test_version()
  local handle = spawn({ bin, "--version" })
  local code = handle:wait()
  -- skip: comrak binary is nix-linked and can't execute on this platform
  if code == 127 or code == 126 then
    lu.skip("binary cannot execute on this platform")
  end
  lu.assertEquals(code, 0)
end
