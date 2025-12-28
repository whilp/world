#!/usr/bin/env lua

local mymodule = require("mymodule")

if not pcall(debug.getlocal, 4, 1) then
  local exit_code = mymodule.main(arg)
  os.exit(exit_code or 0)
end

return mymodule
