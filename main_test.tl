#!/usr/bin/env cosmic
local fs = require("cosmic.fs")
local cio = require("cosmic.io")
local env = require("cosmic.env")
local spawn = require("cosmic.child")

local world = "o/world"

local function test_list()
  local h = spawn.spawn({world, "--list"})
  local ok, out = h:read()
  assert(ok, "world --list failed: " .. tostring(out))
  assert(out:find(".config/example/config"), "expected .config/example/config in list output")
end
test_list()

local function test_help()
  local h = spawn.spawn({world, "--help"})
  local code = h:wait()
  assert(code == 0, "world --help exited " .. tostring(code))
end
test_help()

local function test_extract()
  local tmpdir = env.get("TEST_TMPDIR")
  assert(tmpdir, "TEST_TMPDIR must be set")
  local dest = fs.join(tmpdir, "extract")

  local h = spawn.spawn({world, "--dest", dest})
  local ok, out = h:read()
  assert(ok, "world --dest failed: " .. tostring(out))

  local config_path = fs.join(dest, ".config/example/config")
  assert(fs.isfile(config_path), "expected " .. config_path .. " to exist")

  local content = cio.slurp(config_path)
  assert(content, "failed to read extracted config")
  assert(content:find("key = value"), "expected 'key = value' in config")
end
test_extract()

local function test_extract_no_overwrite_check()
  local tmpdir = env.get("TEST_TMPDIR")
  assert(tmpdir, "TEST_TMPDIR must be set")
  local dest = fs.join(tmpdir, "extract2")

  -- run twice, second should succeed too
  local h = spawn.spawn({world, "--dest", dest})
  local ok, out = h:read()
  assert(ok, "first extract failed: " .. tostring(out))

  h = spawn.spawn({world, "--dest", dest})
  ok, out = h:read()
  assert(ok, "second extract failed: " .. tostring(out))
end
test_extract_no_overwrite_check()

local function test_extract_creates_dirs()
  local tmpdir = env.get("TEST_TMPDIR")
  assert(tmpdir, "TEST_TMPDIR must be set")
  local dest = fs.join(tmpdir, "deep/nested/path")

  local h = spawn.spawn({world, "--dest", dest})
  local ok, out = h:read()
  assert(ok, "world --dest failed: " .. tostring(out))

  assert(fs.isdir(dest), "expected dest dir to be created")
  assert(fs.isfile(fs.join(dest, ".config/example/config")),
    "expected config in nested dest")
end
test_extract_creates_dirs()
