#!/usr/bin/env run-test.lua
-- teal ignore: test file
-- ast-grep ignore: test file needs package.path manipulation

package.path = TEST_DIR .. "/?.lua;" .. package.path
assert(require("luaunit"))
