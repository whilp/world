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

function TestCosmicBinary:test_warnings_flag()
  -- -W should convert warnings to errors
  local ok, out, code = spawn({cosmic, "-W", "-e", "warn('test warning')"}, {env = clean_env()}):read()
  lu.assertFalse(ok, "cosmic should fail with -W when warning is issued")
  lu.assertStrContains(out, "warning: test warning")
  lu.assertEquals(code, 1)
end

function TestCosmicBinary:test_warnings_without_flag()
  -- without -W, warnings should not cause errors
  local ok, out = spawn({cosmic, "-e", "warn('test warning'); print('ok')"}, {env = clean_env()}):read()
  lu.assertTrue(ok, "cosmic should succeed without -W")
  lu.assertStrContains(out, "ok")
end

function TestCosmicBinary:test_interactive_mode()
  -- -i should enter interactive mode (REPL)
  local proc = spawn({cosmic, "-i"}, {env = clean_env(), stdin = "print('hello')\ncont\n"})
  local ok, out = proc:read()
  lu.assertTrue(ok, "cosmic -i should succeed")
  lu.assertStrContains(out, "Lua 5.4")
  lu.assertStrContains(out, "hello")
end

function TestCosmicBinary:test_repl_no_args()
  -- no args should enter REPL
  local proc = spawn({cosmic}, {env = clean_env(), stdin = "print('repl test')\ncont\n"})
  local ok, out = proc:read()
  lu.assertTrue(ok, "cosmic with no args should enter REPL")
  lu.assertStrContains(out, "Lua 5.4")
  lu.assertStrContains(out, "repl test")
end
