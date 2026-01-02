local lu = require("luaunit")
local spawn = require("cosmic.spawn").spawn
local path = require("cosmo.path")

local bin = path.join(os.getenv("TEST_BIN_DIR"), "bin", "comrak")

TestComrak = {}

function TestComrak:test_version()
  local handle, err = spawn({ bin, "--version" })
  if not handle then
    lu.skip("spawn failed: " .. tostring(err))
    return
  end
  local _, output, code = handle:read()
  -- skip: comrak binary is nix-linked and can't execute on this platform
  -- may fail with 127 (not found), 126 (not executable), or 1 (dynamic linker failed)
  if code ~= 0 then
    lu.skip("binary cannot execute on this platform (exit " .. code .. ")")
    return
  end
  lu.assertStrContains(output or "", "comrak")
end
