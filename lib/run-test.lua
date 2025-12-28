local lu = require('luaunit')

if not arg[1] then
  io.stderr:write("usage: run-test.lua <test-file>\n")
  os.exit(1)
end

local test_file = arg[1]

-- Clear arg[1] so luaunit doesn't interpret it as a test filter
arg[0] = test_file
arg[1] = nil

local env = setmetatable({}, { __index = _G, __newindex = _G })
local loader, err = loadfile(test_file, "t", env)
if not loader then
  io.stderr:write("error loading " .. test_file .. ": " .. err .. "\n")
  os.exit(1)
end

loader()
os.exit(lu.LuaUnit.run())
