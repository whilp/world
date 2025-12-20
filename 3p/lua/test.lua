#!/usr/bin/env lua
local script_dir = arg[0]:match("(.*/)")

lu = require('luaunit')

dofile(script_dir .. 'test_modules.lua')
dofile(script_dir .. 'test_funcs.lua')

os.exit(lu.LuaUnit.run())
