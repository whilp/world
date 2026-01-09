#!/usr/bin/env run-test.lua
-- teal ignore: test file

local hook = require("skill.hook")
local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

--------------------------------------------------------------------------------
-- mock helpers
--------------------------------------------------------------------------------

local function mock_stdin(json)
  return {
    read = function(_, mode)
      if mode == "*a" then
        return json
      end
      return nil
    end,
  }
end

local function mock_stdout()
  local buffer = {}
  return {
    write = function(_, data)
      buffer[#buffer + 1] = data
      return true
    end,
    data = function()
      return table.concat(buffer)
    end,
  }
end

--------------------------------------------------------------------------------
-- read_input tests
--------------------------------------------------------------------------------

local function test_read_input_valid()
  local input = {hook_event_name = "SessionStart", cwd = "/tmp"}
  local stdin = mock_stdin(cosmo.EncodeJson(input))
  local result, err = hook.read_input(stdin)
  assert(result, "expected result, got: " .. tostring(err))
  assert(result.hook_event_name == "SessionStart", "expected event name")
  assert(result.cwd == "/tmp", "expected cwd")
end
test_read_input_valid()

local function test_read_input_empty()
  local stdin = mock_stdin("")
  local result, err = hook.read_input(stdin)
  assert(not result, "expected no result")
  assert(err == "no input", "expected no input error")
end
test_read_input_empty()

local function test_read_input_invalid_json()
  local stdin = mock_stdin("not json")
  local result, err = hook.read_input(stdin)
  assert(not result, "expected no result")
  assert(err:match("invalid json"), "expected json error")
end
test_read_input_invalid_json()

--------------------------------------------------------------------------------
-- write_output tests
--------------------------------------------------------------------------------

local function test_write_output()
  local stdout = mock_stdout()
  local output = {continue = true, systemMessage = "hello"}
  hook.write_output(output, stdout)
  local data = stdout.data()
  assert(data:match('"continue"'), "expected continue in output")
end
test_write_output()

local function test_write_output_nil()
  local stdout = mock_stdout()
  hook.write_output(nil, stdout)
  assert(stdout.data() == "", "expected empty output")
end
test_write_output_nil()

--------------------------------------------------------------------------------
-- register and dispatch tests
--------------------------------------------------------------------------------

local function test_dispatch_custom_handler()
  -- register a handler that only handles TestEvent
  hook.register(function(input)
    if input.hook_event_name ~= "TestEvent" then
      return nil
    end
    return {custom = "response", received = input.data}
  end)

  local input = {hook_event_name = "TestEvent", data = "test"}
  local result = hook.dispatch(input)
  assert(result, "expected result")
  assert(result.custom == "response", "expected custom response")
  assert(result.received == "test", "expected received data")
end
test_dispatch_custom_handler()

local function test_dispatch_multiple_handlers()
  -- register handlers that produce output for any event
  hook.register(function(input)
    if input.hook_event_name ~= "MultiEvent" then
      return nil
    end
    return {first = true}
  end)
  hook.register(function(input)
    if input.hook_event_name ~= "MultiEvent" then
      return nil
    end
    return {second = true}
  end)

  local input = {hook_event_name = "MultiEvent"}
  local result = hook.dispatch(input)
  assert(result, "expected result")
  assert(result.first == true, "expected first handler output")
  assert(result.second == true, "expected second handler output")
end
test_dispatch_multiple_handlers()

--------------------------------------------------------------------------------
-- session_start bootstrap tests
--------------------------------------------------------------------------------

local function test_session_start_bootstrap()
  local tmpdir = os.getenv("TEST_TMPDIR") or "/tmp"
  local env_file = path.join(tmpdir, "test_env")

  -- set up env var
  local orig_env_file = os.getenv("CLAUDE_ENV_FILE")
  unix.setenv("CLAUDE_ENV_FILE", env_file)

  -- dispatch session start
  local input = {
    hook_event_name = "SessionStart",
    source = "startup",
    cwd = "/home/user/project",
  }
  local result, err = hook.dispatch(input)
  assert(not err, "expected no error, got: " .. tostring(err))

  -- check file was written
  local f = io.open(env_file, "r")
  if f then
    local content = f:read("*a")
    f:close()
    assert(content:match('PATH="/home/user/project/bin'), "expected PATH export")
  end

  -- cleanup
  os.remove(env_file)
  if orig_env_file then
    unix.setenv("CLAUDE_ENV_FILE", orig_env_file)
  else
    unix.unsetenv("CLAUDE_ENV_FILE")
  end
end
test_session_start_bootstrap()

local function test_session_start_skip_on_resume()
  local tmpdir = os.getenv("TEST_TMPDIR") or "/tmp"
  local env_file = path.join(tmpdir, "test_env_resume")

  -- set up env var
  local orig_env_file = os.getenv("CLAUDE_ENV_FILE")
  unix.setenv("CLAUDE_ENV_FILE", env_file)

  -- dispatch session start with resume source
  local input = {
    hook_event_name = "SessionStart",
    source = "resume",
    cwd = "/home/user/project",
  }
  local result, err = hook.dispatch(input)
  assert(not err, "expected no error")

  -- check file was NOT written
  local f = io.open(env_file, "r")
  if f then
    local content = f:read("*a")
    f:close()
    assert(content == "" or not content:match("PATH"), "expected no PATH export on resume")
  end

  -- cleanup
  os.remove(env_file)
  if orig_env_file then
    unix.setenv("CLAUDE_ENV_FILE", orig_env_file)
  else
    unix.unsetenv("CLAUDE_ENV_FILE")
  end
end
test_session_start_skip_on_resume()

--------------------------------------------------------------------------------
-- run integration test
--------------------------------------------------------------------------------

local function test_run_integration()
  local input = {hook_event_name = "SessionStart", source = "compact"}
  local stdin = mock_stdin(cosmo.EncodeJson(input))
  local stdout = mock_stdout()

  local code = hook.run({stdin = stdin, stdout = stdout})
  assert(code == 0, "expected success")
end
test_run_integration()
