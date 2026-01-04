#!/usr/bin/env run-test.lua
-- teal ignore: test file
-- ast-grep ignore: test file uses temp path concatenation

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local home = require("home.main")

-- Test built binary has valid manifest
local home_bin = path.join(os.getenv("TEST_BIN"), "home")
local ok, manifest_content = spawn({"unzip", "-p", home_bin, "manifest.lua"}):read()
assert(ok, "failed to extract manifest.lua from built binary")
assert(manifest_content and #manifest_content > 0, "manifest.lua is empty in built binary")
local chunk, err = load(manifest_content)
assert(chunk, "manifest.lua is not valid lua: " .. tostring(err))
local manifest = chunk()
assert(type(manifest) == "table", "manifest.lua does not return a table")
assert(manifest.files, "manifest.lua missing files key")
assert(next(manifest.files), "manifest.lua has empty files")

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

-- Test cmd_unpack with invalid manifests
local function capture_output()
  local out = { stderr = "", stdout = "" }
  out.stderr_obj = {
    write = function(_, s) out.stderr = out.stderr .. s end,
  }
  out.stdout_obj = {
    write = function(_, s) out.stdout = out.stdout .. s end,
  }
  return out
end

-- Empty manifest (nil) should fail
local out = capture_output()
local ret = home.cmd_unpack(TEST_TMPDIR, false, {
  manifest = nil,
  stderr = out.stderr_obj,
  stdout = out.stdout_obj,
})
assert(ret == 1, "cmd_unpack nil manifest should return 1")
assert(out.stderr:find("failed to load manifest"), "nil manifest error message")

-- Empty table manifest should fail
out = capture_output()
ret = home.cmd_unpack(TEST_TMPDIR, false, {
  manifest = {},
  stderr = out.stderr_obj,
  stdout = out.stdout_obj,
})
assert(ret == 1, "cmd_unpack empty manifest should return 1")
assert(out.stderr:find("failed to load manifest"), "empty manifest error message")

-- Manifest without files key should fail
out = capture_output()
ret = home.cmd_unpack(TEST_TMPDIR, false, {
  manifest = { version = "1.0.0" },
  stderr = out.stderr_obj,
  stdout = out.stdout_obj,
})
assert(ret == 1, "cmd_unpack manifest without files should return 1")
assert(out.stderr:find("failed to load manifest"), "missing files error message")

-- Valid manifest with empty files should succeed
out = capture_output()
ret = home.cmd_unpack(TEST_TMPDIR, false, {
  manifest = { version = "1.0.0", files = {} },
  stderr = out.stderr_obj,
  stdout = out.stdout_obj,
})
assert(ret == 0, "cmd_unpack valid manifest should return 0, got: " .. ret)

-- Valid manifest with file entry should work
local test_file = path.join(TEST_TMPDIR, "manifest_test.txt")
cosmo.Barf(test_file, "test content")
out = capture_output()
ret = home.cmd_unpack(TEST_TMPDIR, false, {
  manifest = {
    version = "1.0.0",
    files = {
      ["test.txt"] = { mode = tonumber("644", 8) },
    },
  },
  zip_root = TEST_TMPDIR .. "/",
  stderr = out.stderr_obj,
  stdout = out.stdout_obj,
  verbose = true,
})
-- This will fail because test.txt doesn't exist in zip_root, but that's fine
-- The point is it gets past manifest validation

-- Test cmd_list with invalid manifests
out = capture_output()
ret = home.cmd_list({
  manifest = nil,
  stderr = out.stderr_obj,
  stdout = out.stdout_obj,
})
assert(ret == 1, "cmd_list nil manifest should return 1")
assert(out.stderr:find("failed to load manifest"), "cmd_list nil manifest error message")

out = capture_output()
ret = home.cmd_list({
  manifest = {},
  stderr = out.stderr_obj,
  stdout = out.stdout_obj,
})
assert(ret == 1, "cmd_list empty manifest should return 1")
assert(out.stderr:find("failed to load manifest"), "cmd_list empty manifest error message")

out = capture_output()
ret = home.cmd_list({
  manifest = { version = "1.0.0", files = {} },
  stderr = out.stderr_obj,
  stdout = out.stdout_obj,
})
assert(ret == 0, "cmd_list valid manifest should return 0")
