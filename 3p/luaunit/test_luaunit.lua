#!/usr/bin/env run.lua

package.path = "o/3p/luaunit/version.lua.staged/?.lua;" .. package.path
assert(require("luaunit"))
