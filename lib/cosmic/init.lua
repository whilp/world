-- cosmic: cosmopolitan lua utilities
-- require individual modules via require("cosmic.spawn"), require("cosmic.walk"), etc.

-- teal ignore: type annotations needed
local cosmo = require("cosmo")

local function main(fn)
  if not cosmo.is_main() then return end
  local env = {stderr = io.stderr, stdout = io.stdout}
  local code, err = fn(arg, env)
  if err then env.stderr:write(err .. "\n") end
  os.exit(code or 0)
end

return {
  _VERSION = "0.1.0",
  _DESCRIPTION = "Cosmopolitan Lua utilities",
  main = main,
}
