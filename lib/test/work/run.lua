#!/usr/bin/env lua

local lu = require("luaunit")

package.path = os.getenv("HOME") .. "/lib/test/work/?.lua;" .. package.path

require("command-blocked")
require("blockers")
require("blocked-on-display")
require("validate-blocks")
require("orphaned-blocks")
require("file-locking")
require("string-sanitization")
require("backup")

os.exit(lu.LuaUnit.run())
