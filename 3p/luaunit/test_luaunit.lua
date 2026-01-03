#!/usr/bin/env run.lua

local staged = os.getenv("STAGED_DIR")
if staged then
  package.path = staged .. "/?.lua;" .. package.path
end
assert(require("luaunit"))
