#!/usr/bin/env run-test.lua
-- ast-grep ignore: test file

-- This test is slow and only needed for release validation
assert(os.getenv("TEST_RELEASE"), "IGNORE: release validation test")

local path = require("cosmo.path")
local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")

local record SpawnHandle
  stdout: FILE
  stderr: FILE
  read: function(self: SpawnHandle): boolean, string
  wait: function(self: SpawnHandle): integer
end

local home_bin = path.join(os.getenv("TEST_BIN") or "", "home")
local cosmic_bin = path.join(os.getenv("TEST_BIN") or "", "cosmic")
local cosmos_zip = unix.realpath(path.join(os.getenv("TEST_O") or "", "cosmos/.staged/zip"))

local TEST_TMPDIR = os.getenv("TEST_TMPDIR") or "/tmp"

-- Create a release-like home binary with platform metadata
local release_dir = path.join(TEST_TMPDIR, "release")
local platforms_dir = path.join(release_dir, "platforms")
local test_home = path.join(release_dir, "home")
local test_asset = path.join(release_dir, "home-linux-x86_64")

unix.makedirs(platforms_dir)

-- Copy home binary (once as main, once as platform asset)
local ok, err = (spawn({ "cp", home_bin, test_home }) as SpawnHandle):wait()
assert(ok == 0, "failed to copy home binary: " .. tostring(err))
ok, err = (spawn({ "cp", home_bin, test_asset }) as SpawnHandle):wait()
assert(ok == 0, "failed to copy home asset: " .. tostring(err))

-- Generate platform metadata (compiled from Teal)
local gen_platforms = path.join(os.getenv("TEST_O") or "", "lib/home/gen-platforms.lua")

-- Build env with LUA_PATH prepended (unix.environ returns array of "K=V" strings)
local env: {string} = { "LUA_PATH=lib/home/?.lua;;" }
for _, entry in ipairs(unix.environ() as {string}) do
  if not entry:match("^LUA_PATH=") then
    table.insert(env, entry)
  end
end

local output: string
local proc = spawn({
  cosmic_bin, gen_platforms,
  platforms_dir, "https://example.com/releases/test", "test-tag",
  test_asset,
}, { env = env }) as SpawnHandle
local stdout = proc.stdout:read()
local stderr = proc.stderr:read()
local exit_code = proc:wait()
ok = exit_code == 0
assert(ok, "gen-platforms failed (exit " .. tostring(exit_code) .. "): " .. tostring(stderr))

-- Verify platforms.lua was generated
local platforms_lua = path.join(platforms_dir, "platforms.lua")
assert(unix.stat(platforms_lua), "platforms.lua not generated")

-- Embed platforms.lua into test home binary
local read_ok: boolean
read_ok, output = (spawn({ cosmos_zip, "-j", test_home, platforms_lua }) as SpawnHandle):read()
assert(read_ok, "failed to embed platforms.lua: " .. tostring(output))

-- Embed manifests directory (need to cd first since zip stores relative paths)
proc = spawn({ "sh", "-c", "cd " .. platforms_dir .. " && " .. cosmos_zip .. " -r " .. test_home .. " manifests" }) as SpawnHandle
stdout = proc.stdout:read()
stderr = proc.stderr:read()
exit_code = proc:wait()
ok = exit_code == 0
assert(ok, "failed to embed manifests (exit " .. tostring(exit_code) .. "): " .. tostring(stderr) .. " | " .. tostring(stdout))

-- Verify platforms.lua is in the binary
read_ok, output = (spawn({ "unzip", "-l", test_home }) as SpawnHandle):read()
assert(read_ok, "failed to list zip contents")
assert(output:find("platforms%.lua"), "platforms.lua not found in binary")
assert(output:find("manifests/"), "manifests/ not found in binary")

-- Verify --with-platform works (dry-run)
-- Use --platform to override detection since we only embedded linux-x86_64 metadata
read_ok, output = (spawn({ test_home, "unpack", "--with-platform", "--platform", "linux-x86_64", "--dry-run", TEST_TMPDIR }) as SpawnHandle):read()
assert(read_ok, "--with-platform failed: " .. tostring(output))

-- Verify original binary without platform metadata fails
read_ok, output = (spawn({ home_bin, "unpack", "--with-platform", "--dry-run", TEST_TMPDIR }) as SpawnHandle):read()
assert(not read_ok, "original binary should fail with --with-platform")
