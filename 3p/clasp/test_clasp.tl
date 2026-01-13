#!/usr/bin/env run-test.lua

local spawn = require("cosmic.spawn")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

global TEST_BIN: string
global TEST_TMPDIR: string

local bin = path.join(TEST_BIN, "clasp")

local function test_version()
  local ok, got = spawn({ bin, "version" }):read()
  assert(ok, "clasp version failed")
  local want = "0.1.0"
  assert((got as string):find(want, 1, true), "checking clasp version, want: " .. want .. " got: " .. (got as string))
end

local function test_help()
  local ok, got = spawn({ bin, "help" }):read()
  assert(ok, "clasp help failed")
  local want = "text processing utility"
  assert((got as string):find(want, 1, true), "checking clasp help, want: " .. want .. " got: " .. (got as string))
end

local function test_lines()
  local test_file = path.join(TEST_TMPDIR, "test_lines.txt")
  local f = assert(io.open(test_file, "w"))
  f:write("line1\nline2\nline3\n")
  f:close()

  local ok, got = spawn({ bin, "lines", test_file }):read()
  assert(ok, "clasp lines failed")
  local want = "3"
  assert((got as string):match("^%s*(.-)%s*$") == want, "checking clasp lines, want: " .. want .. " got: " .. (got as string))
end

local function test_words()
  local test_file = path.join(TEST_TMPDIR, "test_words.txt")
  local f = assert(io.open(test_file, "w"))
  f:write("hello world foo bar\n")
  f:close()

  local ok, got = spawn({ bin, "words", test_file }):read()
  assert(ok, "clasp words failed")
  local want = "4"
  assert((got as string):match("^%s*(.-)%s*$") == want, "checking clasp words, want: " .. want .. " got: " .. (got as string))
end

local function test_json()
  local test_file = path.join(TEST_TMPDIR, "test.json")
  local f = assert(io.open(test_file, "w"))
  f:write('{"a":1,"b":2}')
  f:close()

  local ok, got = spawn({ bin, "json", test_file }):read()
  assert(ok, "clasp json failed")
  local want = '"a": 1'
  assert((got as string):find(want, 1, true), "checking clasp json, want: " .. want .. " got: " .. (got as string))
end

test_version()
test_help()
test_lines()
test_words()
test_json()
