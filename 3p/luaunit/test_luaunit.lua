#!/usr/bin/env run-test.lua
-- teal ignore: test file
-- ast-grep ignore: test file needs package.path manipulation

local staged = os.getenv("STAGED_DIR")
if staged then
  package.path = staged .. "/?.lua;" .. package.path
end
assert(require("luaunit"))
