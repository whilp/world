-- cosmic: cosmopolitan lua utilities
-- require individual modules via require("cosmic.spawn"), require("cosmic.walk"), etc.

local M = {
  _VERSION = "0.1.0",
  _DESCRIPTION = "Cosmopolitan Lua utilities",
}

-- lazy-load submodules
setmetatable(M, {
  __index = function(t, k)
    local ok, mod = pcall(require, "cosmic." .. k)
    if ok then
      rawset(t, k, mod)
      return mod
    end
    return nil
  end,
})

return M
