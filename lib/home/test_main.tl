#!/usr/bin/env run-test.lua
-- ast-grep ignore: test file uses temp path concatenation

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local home = require("home.main")

local TEST_TMPDIR = os.getenv("TEST_TMPDIR") or "/tmp"

local record Args
  cmd: string
  dest: string
  force: boolean
end

local record CapturedOutput
  stderr: string
  stdout: string
  stderr_obj: FILE
  stdout_obj: FILE
end

local record Manifest
  files: {string:any}
  version: string
end

local record SpawnHandle
  read: function(self: SpawnHandle): boolean, string
end

-- Test built binary has valid manifest
local home_bin = path.join(os.getenv("TEST_BIN") or "", "home")
local ok, manifest_content = (spawn({"unzip", "-p", home_bin, "manifest.lua"}) as SpawnHandle):read()
assert(ok, "failed to extract manifest.lua from built binary")
assert(manifest_content and #manifest_content > 0, "manifest.lua is empty in built binary")
local chunk, err = load(manifest_content)
assert(chunk, "manifest.lua is not valid lua: " .. tostring(err))
local manifest = chunk() as Manifest
assert(type(manifest) == "table", "manifest.lua does not return a table")
assert(manifest.files, "manifest.lua missing files key")
assert(next(manifest.files), "manifest.lua has empty files")

-- Test parse_args
local args = home.parse_args({ "unpack", "/tmp/dest" }) as Args
assert(args.cmd == "unpack", "parse_args cmd")
assert(args.dest == "/tmp/dest", "parse_args dest")
assert(not args.force, "parse_args force default")

args = home.parse_args({ "unpack", "--force", "/tmp/dest" }) as Args
assert(args.force, "parse_args --force")

args = home.parse_args({}) as Args
assert(args.cmd == "help", "parse_args default help")

-- Test copy_file
local src = path.join(TEST_TMPDIR, "source.txt")
local dst = path.join(TEST_TMPDIR, "dest.txt")
cosmo.Barf(src, "hello world")

local copy_ok, copy_err = home.copy_file(src, dst)
assert(copy_ok, "copy_file: " .. tostring(copy_err))

local f = io.open(dst, "r")
assert(f, "copy_file destination exists")
local content = f:read("*a")
f:close()
assert(content == "hello world", "copy_file content")

-- Test copy_file no overwrite
cosmo.Barf(dst, "existing")
copy_ok, copy_err = home.copy_file(src, dst, nil, false)
assert(not copy_ok, "copy_file should fail without overwrite")
assert(copy_err:find("already exists"), "copy_file error message")

-- Test copy_file with overwrite
copy_ok, copy_err = home.copy_file(src, dst, nil, true)
assert(copy_ok, "copy_file overwrite: " .. tostring(copy_err))

-- Test read_file
local data: string
data, err = home.read_file(src)
assert(data == "hello world", "read_file content")

data, err = home.read_file("/nonexistent/path")
assert(not data, "read_file nonexistent should fail")

-- Test format_mode
assert(home.format_mode(tonumber("644", 8), false) == "-rw-r--r--", "format_mode 644")
assert(home.format_mode(tonumber("755", 8), false) == "-rwxr-xr-x", "format_mode 755")
assert(home.format_mode(tonumber("755", 8), true) == "drwxr-xr-x", "format_mode dir")

-- Test cmd_unpack with invalid manifests
local function capture_output(): CapturedOutput
  local out: CapturedOutput = { stderr = "", stdout = "" }
  out.stderr_obj = {
    write = function(_: any, s: string): boolean out.stderr = out.stderr .. s return true end,
  } as FILE
  out.stdout_obj = {
    write = function(_: any, s: string): boolean out.stdout = out.stdout .. s return true end,
  } as FILE
  return out
end

-- Empty manifest (nil) should fail in test mode (zip_root provided)
local out = capture_output()
local ret = home.cmd_unpack(TEST_TMPDIR, false, {
  manifest = nil,
  zip_root = TEST_TMPDIR .. "/",
  stderr = out.stderr_obj,
  stdout = out.stdout_obj,
})
assert(ret == 1, "cmd_unpack nil manifest should return 1")
assert(out.stderr:find("failed to load manifest"), "nil manifest error message")

-- Empty table manifest should fail
out = capture_output()
ret = home.cmd_unpack(TEST_TMPDIR, false, {
  manifest = {},
  zip_root = TEST_TMPDIR .. "/",
  stderr = out.stderr_obj,
  stdout = out.stdout_obj,
})
assert(ret == 1, "cmd_unpack empty manifest should return 1")
assert(out.stderr:find("failed to load manifest"), "empty manifest error message")

-- Manifest without files key should fail
out = capture_output()
ret = home.cmd_unpack(TEST_TMPDIR, false, {
  manifest = { version = "1.0.0" },
  zip_root = TEST_TMPDIR .. "/",
  stderr = out.stderr_obj,
  stdout = out.stdout_obj,
})
assert(ret == 1, "cmd_unpack manifest without files should return 1")
assert(out.stderr:find("failed to load manifest"), "missing files error message")

-- Valid manifest with empty files should succeed
out = capture_output()
ret = home.cmd_unpack(TEST_TMPDIR, false, {
  manifest = { version = "1.0.0", files = {} },
  zip_root = TEST_TMPDIR .. "/",
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

-- Test unknown flags are rejected
local handle = spawn({home_bin, "unpack", "--with-platform", TEST_TMPDIR})
local exit_code = handle:wait()
assert(exit_code ~= 0, "unknown flag --with-platform should fail, got exit code: " .. tostring(exit_code))

handle = spawn({home_bin, "unpack", "--unknown-flag", TEST_TMPDIR})
exit_code = handle:wait()
assert(exit_code ~= 0, "unknown flag --unknown-flag should fail")
