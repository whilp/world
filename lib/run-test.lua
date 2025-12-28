local lu = require('luaunit')

if not arg[1] then
  io.stderr:write("usage: run-test.lua <test-file>\n")
  os.exit(1)
end

local test_file = arg[1]
arg[0] = test_file
arg[1] = nil

dofile(test_file)
os.exit(lu.LuaUnit.run())
