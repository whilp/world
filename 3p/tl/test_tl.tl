#!/usr/bin/env run-test.lua
-- ast-grep ignore: test file needs package.path manipulation
local path = require("cosmo.path")

global TEST_DIR: string

-- add tl.lua to package.path
package.path = path.join(TEST_DIR, "?.lua") .. ";" .. package.path

local function test_load_tl()
  local tl = require("tl")
  assert(tl ~= nil, "failed to load tl")
  assert(type(tl.load) == "function", "tl.load should be a function")
  assert(type(tl.gen) == "function", "tl.gen should be a function")
end
test_load_tl()

local function test_parse_simple()
  local tl = require("tl")
  local result = tl.process_string("local x: number = 1")
  assert(result ~= nil, "tl.process_string should return a result")
  assert(result.syntax_errors == nil or #result.syntax_errors == 0, "should have no syntax errors")
end
test_parse_simple()
