#!/usr/bin/env lua

local cosmo = require("cosmo")
local unix = cosmo.unix

-- Build path to src directory relative to this script
-- Script is in .local/bin, src is at project root
local script_dir = cosmo.path.dirname(cosmo.path.dirname(cosmo.path.dirname(debug.getinfo(1, "S").source:sub(2))))
package.path = script_dir .. "/src/?.lua;" .. package.path

-- Replace 'mymodule' with your module name
local mymodule = require("mymodule.main")

-- Only run main if this script is executed directly (not required as a module)
if not pcall(debug.getlocal, 4, 1) then
  local exit_code = mymodule.main(arg)
  os.exit(exit_code or 0)
end

-- Return module to support testing
return mymodule
