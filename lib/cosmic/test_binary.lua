#!/usr/bin/env run-test.lua
-- teal ignore: test file
-- test cosmic bundled binary

local lu = require("luaunit")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local cosmic = path.join(os.getenv("TEST_BIN"), "cosmic")

-- helper to create clean environment without LUA_PATH
-- this ensures we test the bundled libraries, not the repo
local function clean_env()
  local env = {}
  for _, line in ipairs(unix.environ()) do
    if not line:match("^LUA_PATH=") and not line:match("^LUA_CPATH=") then
      env[#env + 1] = line
    end
  end
  return env
end

TestCosmicBinary = {}

function TestCosmicBinary:test_cosmic_spawn()
  local ok, out = spawn({cosmic, "-e", "local s = require('cosmic.spawn'); print(s and 'ok' or 'fail')"}, {env = clean_env()}):read()
  lu.assertTrue(ok, "cosmic exited with error")
  lu.assertStrContains(out, "ok")
end

function TestCosmicBinary:test_cosmic_walk()
  local ok, out = spawn({cosmic, "-e", "local w = require('cosmic.walk'); print(w and 'ok' or 'fail')"}, {env = clean_env()}):read()
  lu.assertTrue(ok, "cosmic exited with error")
  lu.assertStrContains(out, "ok")
end

function TestCosmicBinary:test_cosmic_help()
  local ok, out = spawn({cosmic, "-e", "require('cosmic.help')"}, {env = clean_env()}):read()
  lu.assertTrue(ok, "cosmic exited with error")
  lu.assertStrContains(out, "cosmic")
  lu.assertStrContains(out, "cosmic.spawn")
end

function TestCosmicBinary:test_luaunit_bundled()
  local ok, out = spawn({cosmic, "-e", "local lu = require('luaunit'); print(lu and 'ok' or 'fail')"}, {env = clean_env()}):read()
  lu.assertTrue(ok, "cosmic exited with error")
  lu.assertStrContains(out, "ok")
end

function TestCosmicBinary:test_argparse_bundled()
  local ok, out = spawn({cosmic, "-e", "local ap = require('argparse'); print(ap and 'ok' or 'fail')"}, {env = clean_env()}):read()
  lu.assertTrue(ok, "cosmic exited with error")
  lu.assertStrContains(out, "ok")
end

function TestCosmicBinary:test_lfs_bundled()
  local ok, out = spawn({cosmic, "-e", "local lfs = require('lfs'); print(lfs and 'ok' or 'fail')"}, {env = clean_env()}):read()
  lu.assertTrue(ok, "cosmic exited with error")
  lu.assertStrContains(out, "ok")
end
