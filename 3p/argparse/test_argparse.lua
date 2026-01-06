#!/usr/bin/env run-test.lua
-- teal ignore: test file
-- ast-grep ignore: test file needs package.path manipulation

local path = require("cosmo.path")

-- add staged dir to package.path
package.path = path.join(TEST_DIR, "?.lua") .. ";" .. package.path

local argparse = require("argparse")

local function test_create_parser()
  local parser = argparse("test", "A test program")
  assert(parser, "failed to create parser")
end
test_create_parser()

local function test_parse_args()
  local parser = argparse("test", "A test program")
  parser:argument("input", "Input file")
  local args = parser:parse({ "foo.txt" })
  assert(args.input == "foo.txt", "expected 'foo.txt', got: " .. tostring(args.input))
end
test_parse_args()
