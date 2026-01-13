#!/usr/bin/env run-test.lua

local hook = require("skill.hook")
local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

local record MockStdin
  read: function(self: MockStdin, mode: string): string
end

local record MockStdout
  write: function(self: MockStdout, data: string): boolean
  data: function(self: MockStdout): string
end

local record HookInput
  hook_event_name: string
  source: string
  cwd: string
  tool_name: string
  tool_input: ToolInput
  data: string
end

local record ToolInput
  command: string
end

local record HookOutput
  continue: boolean
  systemMessage: string
  custom: string
  received: string
  first: boolean
  second: boolean
  reason: string
  hookSpecificOutput: HookSpecificOutput
end

local record HookSpecificOutput
  postToolUse: PostToolUse
end

local record PostToolUse
  additionalContext: string
end

--------------------------------------------------------------------------------
-- mock helpers
--------------------------------------------------------------------------------

local function mock_stdin(json: string): MockStdin
  return {
    read = function(_: MockStdin, mode: string): string
      if mode == "*a" then
        return json
      end
      return nil
    end,
  }
end

local function mock_stdout(): MockStdout
  local buffer: {string} = {}
  return {
    write = function(_: MockStdout, data: string): boolean
      buffer[#buffer + 1] = data
      return true
    end,
    data = function(_: MockStdout): string
      return table.concat(buffer)
    end,
  }
end

--------------------------------------------------------------------------------
-- read_input tests
--------------------------------------------------------------------------------

local function test_read_input_valid()
  local input: HookInput = {hook_event_name = "SessionStart", cwd = "/tmp"}
  local stdin = mock_stdin(cosmo.EncodeJson(input))
  local result, err = hook.read_input(stdin as FILE)
  assert(result, "expected result, got: " .. tostring(err))
  local r = result as HookInput
  assert(r.hook_event_name == "SessionStart", "expected event name")
  assert(r.cwd == "/tmp", "expected cwd")
end
test_read_input_valid()

local function test_read_input_empty()
  local stdin = mock_stdin("")
  local result, err = hook.read_input(stdin as FILE)
  assert(not result, "expected no result")
  assert(err == "no input", "expected no input error")
end
test_read_input_empty()

local function test_read_input_invalid_json()
  local stdin = mock_stdin("not json")
  local result, err = hook.read_input(stdin as FILE)
  assert(not result, "expected no result")
  assert(err:match("invalid json"), "expected json error")
end
test_read_input_invalid_json()

--------------------------------------------------------------------------------
-- write_output tests
--------------------------------------------------------------------------------

local function test_write_output()
  local stdout = mock_stdout()
  local output: HookOutput = {continue = true, systemMessage = "hello"}
  hook.write_output(output, stdout as FILE)
  local data = stdout.data()
  assert(data:match('"continue"'), "expected continue in output")
end
test_write_output()

local function test_write_output_nil()
  local stdout = mock_stdout()
  hook.write_output(nil, stdout as FILE)
  assert(stdout.data() == "", "expected empty output")
end
test_write_output_nil()

--------------------------------------------------------------------------------
-- register and dispatch tests
--------------------------------------------------------------------------------

local function test_dispatch_custom_handler()
  -- register a handler that only handles TestEvent
  hook.register(function(input: HookInput): HookOutput, string
    if input.hook_event_name ~= "TestEvent" then
      return nil
    end
    return {custom = "response", received = input.data}
  end)

  local input: HookInput = {hook_event_name = "TestEvent", data = "test"}
  local result = hook.dispatch(input) as HookOutput
  assert(result, "expected result")
  assert(result.custom == "response", "expected custom response")
  assert(result.received == "test", "expected received data")
end
test_dispatch_custom_handler()

local function test_dispatch_multiple_handlers()
  -- register handlers that produce output for any event
  hook.register(function(input: HookInput): HookOutput, string
    if input.hook_event_name ~= "MultiEvent" then
      return nil
    end
    return {first = true}
  end)
  hook.register(function(input: HookInput): HookOutput, string
    if input.hook_event_name ~= "MultiEvent" then
      return nil
    end
    return {second = true}
  end)

  local input: HookInput = {hook_event_name = "MultiEvent"}
  local result = hook.dispatch(input) as HookOutput
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
  local input: HookInput = {
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
  local input: HookInput = {
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
  local input: HookInput = {hook_event_name = "SessionStart", source = "compact"}
  local stdin = mock_stdin(cosmo.EncodeJson(input))
  local stdout = mock_stdout()

  local code = hook.run({stdin = stdin as FILE, stdout = stdout as FILE})
  assert(code == 0, "expected success")
end
test_run_integration()

--------------------------------------------------------------------------------
-- post_commit_pr_reminder tests
--------------------------------------------------------------------------------

local function test_post_commit_pr_reminder_skip_non_bash()
  local input: HookInput = {hook_event_name = "PostToolUse", tool_name = "Edit"}
  local result = hook.dispatch(input) as HookOutput
  -- should not produce pr reminder output for non-Bash tools
  assert(not result or not result.hookSpecificOutput, "expected no output for Edit tool")
end
test_post_commit_pr_reminder_skip_non_bash()

local function test_post_commit_pr_reminder_skip_non_commit()
  local input: HookInput = {
    hook_event_name = "PostToolUse",
    tool_name = "Bash",
    tool_input = {command = "ls -la"},
  }
  local result = hook.dispatch(input) as HookOutput
  -- should not produce pr reminder for non-commit commands
  assert(not result or not result.hookSpecificOutput, "expected no output for ls command")
end
test_post_commit_pr_reminder_skip_non_commit()

--------------------------------------------------------------------------------
-- post_push_check_reminder tests
--------------------------------------------------------------------------------

local function test_post_push_check_reminder_on_feature_branch()
  local input: HookInput = {
    hook_event_name = "PostToolUse",
    tool_name = "Bash",
    tool_input = {command = "git push -u origin feature-branch"},
  }
  local result = hook.dispatch(input) as HookOutput
  -- we're on a feature branch, so should get reminder
  if result and result.hookSpecificOutput then
    local ctx = result.hookSpecificOutput.postToolUse
    assert(ctx and ctx.additionalContext:match("make help"), "expected make help in reminder")
  end
end
test_post_push_check_reminder_on_feature_branch()

local function test_post_push_check_reminder_skip_non_push()
  local input: HookInput = {
    hook_event_name = "PostToolUse",
    tool_name = "Bash",
    tool_input = {command = "git status"},
  }
  local result = hook.dispatch(input) as HookOutput
  -- should not produce check reminder for non-push commands
  local has_check_reminder = result and result.hookSpecificOutput
    and result.hookSpecificOutput.postToolUse
    and result.hookSpecificOutput.postToolUse.additionalContext
    and result.hookSpecificOutput.postToolUse.additionalContext:match("checks")
  assert(not has_check_reminder, "expected no check reminder for git status")
end
test_post_push_check_reminder_skip_non_push()

--------------------------------------------------------------------------------
-- dispatch reason concatenation tests
--------------------------------------------------------------------------------

local function test_dispatch_concatenates_reasons()
  -- register two handlers that return different reasons
  hook.register(function(input: HookInput): HookOutput, string
    if input.hook_event_name ~= "ConcatTest" then
      return nil
    end
    return {reason = "first reason"}
  end)
  hook.register(function(input: HookInput): HookOutput, string
    if input.hook_event_name ~= "ConcatTest" then
      return nil
    end
    return {reason = "second reason"}
  end)

  local input: HookInput = {hook_event_name = "ConcatTest"}
  local result = hook.dispatch(input) as HookOutput
  assert(result, "expected result")
  assert(result.reason, "expected reason")
  assert(result.reason:match("first reason"), "expected first reason")
  assert(result.reason:match("second reason"), "expected second reason")
  assert(result.reason:match("\n"), "expected newline between reasons")
end
test_dispatch_concatenates_reasons()

local function test_dispatch_concatenates_additional_context()
  -- register two handlers that return different additionalContext
  hook.register(function(input: HookInput): HookOutput, string
    if input.hook_event_name ~= "ContextConcatTest" then
      return nil
    end
    return {
      hookSpecificOutput = {
        postToolUse = {additionalContext = "first context message"},
      },
    }
  end)
  hook.register(function(input: HookInput): HookOutput, string
    if input.hook_event_name ~= "ContextConcatTest" then
      return nil
    end
    return {
      hookSpecificOutput = {
        postToolUse = {additionalContext = "second context message"},
      },
    }
  end)

  local input: HookInput = {hook_event_name = "ContextConcatTest"}
  local result = hook.dispatch(input) as HookOutput
  assert(result, "expected result")
  assert(result.hookSpecificOutput, "expected hookSpecificOutput")
  assert(result.hookSpecificOutput.postToolUse, "expected postToolUse")
  local ctx = result.hookSpecificOutput.postToolUse.additionalContext
  assert(ctx, "expected additionalContext")
  assert(ctx:match("first context message"), "expected first context")
  assert(ctx:match("second context message"), "expected second context")
  assert(ctx:match("\n"), "expected newline between contexts")
end
test_dispatch_concatenates_additional_context()

--------------------------------------------------------------------------------
-- post_push_pr_check tests
--------------------------------------------------------------------------------

local function test_post_push_hooks_on_git_push()
  -- on feature branch, both post_push handlers fire for git push
  local input: HookInput = {
    hook_event_name = "PostToolUse",
    tool_name = "Bash",
    tool_input = {command = "git push -u origin feature-branch"},
  }
  local result = hook.dispatch(input) as HookOutput
  -- we're on a feature branch so should get output
  if result and result.hookSpecificOutput then
    local ctx = result.hookSpecificOutput.postToolUse
    if ctx and ctx.additionalContext then
      -- should contain both the PR hint and make help reminder
      local has_pr = ctx.additionalContext:match("PR") or ctx.additionalContext:match("pr") or ctx.additionalContext:match("trailer")
      local has_help = ctx.additionalContext:match("make help")
      assert(has_pr or has_help, "expected PR hint or make help reminder")
    end
  end
end
test_post_push_hooks_on_git_push()

local function test_post_push_skip_non_push()
  local input: HookInput = {
    hook_event_name = "PostToolUse",
    tool_name = "Bash",
    tool_input = {command = "git fetch origin"},
  }
  local result = hook.dispatch(input) as HookOutput
  -- should not produce pr check output for non-push commands
  local has_pr_output = result and result.hookSpecificOutput
    and result.hookSpecificOutput.postToolUse
    and result.hookSpecificOutput.postToolUse.additionalContext
    and (result.hookSpecificOutput.postToolUse.additionalContext:match("PR")
      or result.hookSpecificOutput.postToolUse.additionalContext:match("trailer"))
  assert(not has_pr_output, "expected no PR output for git fetch")
end
test_post_push_skip_non_push()
