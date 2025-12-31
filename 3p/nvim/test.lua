#!/usr/bin/env lua
local spawn = require("spawn").spawn

local function main(bin_dir)
  local bin = bin_dir .. "/bin/nvim"
  local handle = spawn({bin, "--version"})
  return handle:wait() == 0
end

if not pcall(debug.getlocal, 4, 1) then
  if not main(...) then os.exit(1) end
end
