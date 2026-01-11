#!/usr/bin/env run-test.lua
-- ast-grep ignore: test file needs package.path manipulation

global TEST_DIR: string

package.path = TEST_DIR .. "/?.lua;" .. package.path
assert(require("luaunit"))
