#!/usr/bin/env cosmic
local fs = require("cosmic.fs")
local cio = require("cosmic.io")
local env = require("cosmic.env")
local spawn = require("cosmic.child")

local world = "o/world"

--- unit tests: call list/extract with embed/ as root

local main = require("main")
local embed_root = "embed/"

local function test_usage()
  local s = main.usage()
  assert(s:find("--dest"), "expected --dest in usage")
  assert(s:find("--list"), "expected --list in usage")
  assert(s:find("--help"), "expected --help in usage")
end
test_usage()

local function test_list_returns_sorted_paths()
  local files = main.list(embed_root)
  assert(#files > 0, "expected at least one file")
  for i = 2, #files do
    assert(files[i - 1] <= files[i], "list not sorted: " .. files[i - 1] .. " > " .. files[i])
  end
end
test_list_returns_sorted_paths()

local function test_list_contains_config()
  local files = main.list(embed_root)
  local found = false
  for _, f in ipairs(files) do
    if f == ".config/example/config" then
      found = true
      break
    end
  end
  assert(found, "expected .config/example/config in list")
end
test_list_contains_config()

local function test_extract_writes_files()
  local tmpdir = env.get("TEST_TMPDIR")
  assert(tmpdir, "TEST_TMPDIR must be set")
  local dest = fs.join(tmpdir, "unit_extract")

  local results, err = main.extract(dest, embed_root)
  assert(results, "extract failed: " .. tostring(err))
  assert(#results > 0, "expected at least one result")

  for _, r in ipairs(results) do
    assert(fs.isfile(r.target), "expected " .. r.target .. " to exist")
  end
end
test_extract_writes_files()

local function test_extract_content()
  local tmpdir = env.get("TEST_TMPDIR")
  assert(tmpdir, "TEST_TMPDIR must be set")
  local dest = fs.join(tmpdir, "unit_content")

  local results, err = main.extract(dest, embed_root)
  assert(results, "extract failed: " .. tostring(err))

  local config_path = fs.join(dest, ".config/example/config")
  local content = cio.slurp(config_path)
  assert(content, "failed to read config")
  assert(content:find("key = value"), "expected 'key = value' in config")
end
test_extract_content()

local function test_extract_idempotent()
  local tmpdir = env.get("TEST_TMPDIR")
  assert(tmpdir, "TEST_TMPDIR must be set")
  local dest = fs.join(tmpdir, "unit_idempotent")

  local r1, e1 = main.extract(dest, embed_root)
  assert(r1, "first extract failed: " .. tostring(e1))

  local r2, e2 = main.extract(dest, embed_root)
  assert(r2, "second extract failed: " .. tostring(e2))
  assert(#r1 == #r2, "expected same number of results")
end
test_extract_idempotent()

local function test_extract_creates_deep_dirs()
  local tmpdir = env.get("TEST_TMPDIR")
  assert(tmpdir, "TEST_TMPDIR must be set")
  local dest = fs.join(tmpdir, "deep/nested/path")

  local results, err = main.extract(dest, embed_root)
  assert(results, "extract failed: " .. tostring(err))
  assert(fs.isdir(dest), "expected dest dir to be created")
end
test_extract_creates_deep_dirs()

--- integration tests: run the world binary

local function test_cli_list()
  local h = spawn.spawn({world, "--list"})
  local ok, out = h:read()
  assert(ok, "world --list failed: " .. tostring(out))
  assert(out:find(".config/example/config"), "expected .config/example/config in list output")
end
test_cli_list()

local function test_cli_help()
  local h = spawn.spawn({world, "--help"})
  local code = h:wait()
  assert(code == 0, "world --help exited " .. tostring(code))
end
test_cli_help()

local function test_cli_extract()
  local tmpdir = env.get("TEST_TMPDIR")
  assert(tmpdir, "TEST_TMPDIR must be set")
  local dest = fs.join(tmpdir, "cli_extract")

  local h = spawn.spawn({world, "--dest", dest})
  local ok, out = h:read()
  assert(ok, "world --dest failed: " .. tostring(out))

  assert(fs.isfile(fs.join(dest, ".config/example/config")),
    "expected config file to exist")
end
test_cli_extract()
