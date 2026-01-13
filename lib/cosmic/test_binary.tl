#!/usr/bin/env run-test.lua
-- test cosmic bundled binary

local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local cosmic = path.join(os.getenv("TEST_BIN"), "cosmic")

-- helper to create clean environment without LUA_PATH
-- this ensures we test the bundled libraries, not the repo
local function clean_env(): {string}
  local env: {string} = {}
  for _, line in ipairs(unix.environ()) do
    if not line:match("^LUA_PATH=") and not line:match("^LUA_CPATH=") then
      env[#env + 1] = line
    end
  end
  return env
end

local function test_cosmic_spawn()
  local ok, out = spawn({cosmic, "-e", "local s = require('cosmic.spawn'); print(s and 'ok' or 'fail')"}, {env = clean_env()}):read()
  assert(ok, "cosmic exited with error")
  assert((out as string):find("ok", 1, true), "expected output to contain 'ok'")
end
test_cosmic_spawn()

local function test_cosmic_walk()
  local ok, out = spawn({cosmic, "-e", "local w = require('cosmic.walk'); print(w and 'ok' or 'fail')"}, {env = clean_env()}):read()
  assert(ok, "cosmic exited with error")
  assert((out as string):find("ok", 1, true), "expected output to contain 'ok'")
end
test_cosmic_walk()

local function test_cosmic_help()
  local ok, out = spawn({cosmic, "-e", "require('cosmic.help')"}, {env = clean_env()}):read()
  assert(ok, "cosmic exited with error")
  assert((out as string):find("cosmic", 1, true), "expected output to contain 'cosmic'")
  assert((out as string):find("cosmic.spawn", 1, true), "expected output to contain 'cosmic.spawn'")
end
test_cosmic_help()

local function test_warnings_flag()
  -- -W should convert warnings to errors
  -- stderr = 1 merges stderr into stdout so we can capture the warning message
  local ok, out, code = spawn({cosmic, "-W", "-e", "warn('test warning')"}, {env = clean_env(), stderr = 1}):read()
  assert(not ok, "cosmic should fail with -W when warning is issued")
  assert((out as string):find("warning: test warning", 1, true), "expected output to contain 'warning: test warning'")
  assert(code == 1, "expected exit code 1, got " .. tostring(code))
end
test_warnings_flag()

local function test_warnings_without_flag()
  -- without -W, warnings should not cause errors
  local ok, out = spawn({cosmic, "-e", "warn('test warning'); print('ok')"}, {env = clean_env()}):read()
  assert(ok, "cosmic should succeed without -W")
  assert((out as string):find("ok", 1, true), "expected output to contain 'ok'")
end
test_warnings_without_flag()

local function test_interactive_mode()
  -- -i should enter interactive mode (REPL)
  local proc = spawn({cosmic, "-i"}, {env = clean_env(), stdin = "print('hello')\ncont\n"})
  local ok, out = proc:read()
  assert(ok, "cosmic -i should succeed")
  assert((out as string):find("Lua 5.4", 1, true), "expected output to contain 'Lua 5.4'")
  assert((out as string):find("hello", 1, true), "expected output to contain 'hello'")
end
test_interactive_mode()

local function test_repl_no_args()
  -- no args should enter REPL
  local proc = spawn({cosmic}, {env = clean_env(), stdin = "print('repl test')\ncont\n"})
  local ok, out = proc:read()
  assert(ok, "cosmic with no args should enter REPL")
  assert((out as string):find("Lua 5.4", 1, true), "expected output to contain 'Lua 5.4'")
  assert((out as string):find("repl test", 1, true), "expected output to contain 'repl test'")
end
test_repl_no_args()
