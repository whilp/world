#!/usr/bin/env lua

local cosmo = require("cosmo")
local path = cosmo.path

local script_dir = path.dirname(path.dirname(debug.getinfo(1, "S").source:sub(2)))
package.path = path.join(script_dir, "lib/?.lua;") .. package.path

local mymodule = require("mymodule")

if not pcall(debug.getlocal, 4, 1) then
  local exit_code = mymodule.main(arg)
  os.exit(exit_code or 0)
end

return mymodule
