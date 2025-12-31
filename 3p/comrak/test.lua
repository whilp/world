#!/usr/bin/env lua
local spawn = require("spawn").spawn

local function main(bin_dir)
  local bin = bin_dir .. "/bin/comrak"

  local handle = spawn({bin, "--version"})
  local code = handle:wait()
  -- skip: comrak binary is nix-linked and can't execute on this platform
  if code == 127 or code == 126 then
    print("SKIP: binary cannot execute on this platform")
    return true
  end
  return code == 0
end

if not pcall(debug.getlocal, 4, 1) then
  if not main(...) then os.exit(1) end
end
