#!/usr/bin/env luajit

local lu = require("luaunit")

package.path = os.getenv("HOME") .. "/.local/lib/lua/?.lua;"
  .. os.getenv("HOME") .. "/.local/lib/lua/test/work/?.lua;" .. package.path

require("command-blocked")
require("blockers")
require("blocked-on-display")
require("validate-blocks")
require("orphaned-blocks")
require("file-locking")
require("string-sanitization")
require("backup")

os.exit(lu.LuaUnit.run())
