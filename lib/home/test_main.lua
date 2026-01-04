#!/usr/bin/env run-test.lua
-- teal ignore: test file
-- ast-grep ignore: test file uses temp path concatenation

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local home = require("home.main")

-- Test parse_args
local args = home.parse_args({ "unpack", "/tmp/dest" })
assert(args.cmd == "unpack", "parse_args cmd")
assert(args.dest == "/tmp/dest", "parse_args dest")
assert(not args.force, "parse_args force default")

args = home.parse_args({ "unpack", "--force", "/tmp/dest" })
assert(args.force, "parse_args --force")

args = home.parse_args({})
assert(args.cmd == "help", "parse_args default help")

-- Test copy_file
local src = path.join(TEST_TMPDIR, "source.txt")
local dst = path.join(TEST_TMPDIR, "dest.txt")
cosmo.Barf(src, "hello world")

local ok, err = home.copy_file(src, dst)
assert(ok, "copy_file: " .. tostring(err))

local f = io.open(dst, "r")
assert(f, "copy_file destination exists")
local content = f:read("*a")
f:close()
assert(content == "hello world", "copy_file content")

-- Test copy_file no overwrite
cosmo.Barf(dst, "existing")
ok, err = home.copy_file(src, dst, nil, false)
assert(not ok, "copy_file should fail without overwrite")
assert(err:find("already exists"), "copy_file error message")

-- Test copy_file with overwrite
ok, err = home.copy_file(src, dst, nil, true)
assert(ok, "copy_file overwrite: " .. tostring(err))

-- Test read_file
local data, err = home.read_file(src)
assert(data == "hello world", "read_file content")

data, err = home.read_file("/nonexistent/path")
assert(not data, "read_file nonexistent should fail")

-- Test format_mode
assert(home.format_mode(tonumber("644", 8), false) == "-rw-r--r--", "format_mode 644")
assert(home.format_mode(tonumber("755", 8), false) == "-rwxr-xr-x", "format_mode 755")
assert(home.format_mode(tonumber("755", 8), true) == "drwxr-xr-x", "format_mode dir")

-- Test detect_platform
local platform = home.detect_platform()
assert(platform, "detect_platform returns value")
assert(
  platform == "darwin-arm64" or platform == "linux-arm64" or platform == "linux-x86_64",
  "detect_platform valid: " .. platform
)
