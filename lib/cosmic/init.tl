-- cosmic: cosmopolitan lua utilities
-- require individual modules via require("cosmic.spawn"), require("cosmic.walk"), etc.

local cosmo = require("cosmo")

local record Env
  stdout: FILE
  stderr: FILE
end

local type MainFn = function(args: {string}, env: Env): number, string

local function main(fn: MainFn)
  if not cosmo.is_main() then return end
  local env: Env = {stderr = io.stderr, stdout = io.stdout}
  local code, err = fn(arg, env)
  if err then env.stderr:write(err .. "\n") end
  os.exit(code or 0)
end

local record cosmic
  _VERSION: string
  _DESCRIPTION: string
  main: function(fn: MainFn)
end

local M: cosmic = {
  _VERSION = "0.1.0",
  _DESCRIPTION = "Cosmopolitan Lua utilities",
  main = main,
}

return M
