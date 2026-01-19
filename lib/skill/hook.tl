-- hook - claude code hook dispatcher
-- receives hook events via stdin JSON, dispatches to registered handlers
-- usage: cosmic /zip/.lua/skill/hook.lua

local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

-- HookInput and HookOutput can have arbitrary fields for extensibility
local type HookInput = {string:any}
local type HookOutput = {string:any}

local type Handler = function(input: HookInput): HookOutput, string

local record RunOpts
  stdin: FILE
  stdout: FILE
end

-- list of handlers, each decides whether to handle based on input
-- handler signature: function(input) -> output_table|nil, error_string|nil
local handlers: {Handler} = {}

local function register(handler: Handler)
  handlers[#handlers + 1] = handler
end

local function read_input(stdin: FILE): HookInput, string
  stdin = stdin or io.stdin
  local raw = stdin:read("*a")
  if not raw or raw == "" then
    return nil, "no input"
  end
  local input = cosmo.DecodeJson(raw) as HookInput
  if not input then
    return nil, "invalid json"
  end
  return input
end

local function write_output(output: HookOutput, stdout: FILE)
  stdout = stdout or io.stdout
  if output then
    stdout:write(cosmo.EncodeJson(output))
  end
end

local function dispatch(input: HookInput): HookOutput, string
  local outputs: {HookOutput} = {}
  for _, handler in ipairs(handlers) do
    local output, err = handler(input)
    if err then
      return nil, err
    end
    if output then
      outputs[#outputs + 1] = output
    end
  end

  if #outputs == 0 then
    return nil
  end

  -- merge outputs: concatenate reason and additionalContext, last wins for others
  local merged: HookOutput = {}
  local reasons: {string} = {}
  local contexts: {string} = {}
  for _, out in ipairs(outputs) do
    -- copy all fields from output (last wins for duplicate keys)
    for k, v in pairs(out as {string:any}) do
      if k == "reason" then
        reasons[#reasons + 1] = v as string
      elseif k == "hookSpecificOutput" then
        local hso = v as {string:any}
        if hso and hso.postToolUse then
          local ptu = hso.postToolUse as {string:any}
          if ptu and ptu.additionalContext then
            contexts[#contexts + 1] = ptu.additionalContext as string
          end
        end
        merged[k] = v
      else
        merged[k] = v
      end
    end
  end

  if #reasons > 0 then
    merged.reason = table.concat(reasons, "\n")
  end

  if #contexts > 1 and merged.hookSpecificOutput then
    local hso = merged.hookSpecificOutput as {string:any}
    if hso and hso.postToolUse then
      local ptu = hso.postToolUse as {string:any}
      if ptu then
        ptu.additionalContext = table.concat(contexts, "\n")
      end
    end
  end

  return merged
end

local function run(opts?: RunOpts): number, string
  opts = opts or {}
  local input, err = read_input(opts.stdin)
  if not input then
    return 0
  end

  local output: HookOutput
  output, err = dispatch(input)
  if err then
    io.stderr:write("hook: " .. err .. "\n")
    return 2, err
  end

  write_output(output, opts.stdout)
  return 0
end

local function main(): number
  return run()
end

--------------------------------------------------------------------------------
-- built-in handlers
--------------------------------------------------------------------------------

local function spawn_capture(cmd: {string}): string
  local spawn = require("cosmic.spawn").spawn
  local handle = spawn(cmd)
  local ok, out = handle:read()
  if not ok then
    return nil
  end
  return out and (out as string):match("^%s*(.-)%s*$")
end

local function session_start_bootstrap(input: HookInput): HookOutput, string
  if input.hook_event_name ~= "SessionStart" then
    return nil
  end
  if input.source and input.source ~= "startup" then
    return nil
  end

  local env_file = os.getenv("CLAUDE_ENV_FILE")
  if not env_file then
    return nil
  end

  local cwd = input.cwd or unix.getcwd()
  if not cwd then
    return nil, "failed to get cwd"
  end

  local bin_path = path.join(cwd, "bin")
  local export_line = string.format('export PATH="%s:$PATH"\n', bin_path)

  local f, err = io.open(env_file, "a")
  if not f then
    return nil, string.format("failed to open %s: %s", env_file, err)
  end

  local ok, write_err = f:write(export_line)
  f:close()

  if not ok then
    return nil, string.format("failed to write to %s: %s", env_file, write_err)
  end

  return nil
end
register(session_start_bootstrap)

local function session_start_make_help(input: HookInput): HookOutput, string
  if input.hook_event_name ~= "SessionStart" then
    return nil
  end
  if input.source and input.source ~= "startup" then
    return nil
  end

  -- use bin/make to ensure cosmo-make is available
  local cwd = input.cwd or unix.getcwd()
  local make = path.join(cwd, "bin/make")

  -- bootstrap first
  local spawn = require("cosmic.spawn").spawn
  local bootstrap = spawn({make, "bootstrap"})
  bootstrap:wait()

  local help = spawn_capture({make, "help"})
  if not help or help == "" then
    return nil
  end

  io.stdout:write(help .. "\n")
  return nil
end
register(session_start_make_help)

local function session_start_pr_reminder(input: HookInput): HookOutput, string
  if input.hook_event_name ~= "SessionStart" then
    return nil
  end
  if input.source and input.source ~= "startup" then
    return nil
  end

  local branch = spawn_capture({"git", "rev-parse", "--abbrev-ref", "HEAD"})
  if not branch or branch == "main" or branch == "master" then
    return nil
  end

  local trailer = spawn_capture({"git", "log", "-1", "--format=%(trailers:key=x-cosmic-pr-name,valueonly)"})
  local has_trailer = trailer and trailer ~= ""

  local msg: string
  if has_trailer then
    local pr_file = path.join(".github/pr", trailer)
    if path.exists(pr_file) then
      msg = string.format("PR file: %s - update if changes diverge from description", pr_file)
    else
      msg = string.format("Create PR file: %s", pr_file)
    end
  else
    msg = "On feature branch without PR file - add x-cosmic-pr-name trailer and .github/pr/<name>.md when ready"
  end

  io.stdout:write(msg .. "\n")
  return nil
end
register(session_start_pr_reminder)

local function post_commit_pr_reminder(input: HookInput): HookOutput, string
  if input.hook_event_name ~= "PostToolUse" then
    return nil
  end
  if input.tool_name ~= "Bash" then
    return nil
  end

  local tool_input = input.tool_input or {} as ToolInput
  local command = tool_input.command or ""
  if not command:match("git commit") then
    return nil
  end

  local branch = spawn_capture({"git", "rev-parse", "--abbrev-ref", "HEAD"})
  if not branch or branch == "main" or branch == "master" then
    return nil
  end

  local has_trailer = spawn_capture({"git", "log", "-1", "--format=%(trailers:key=x-cosmic-pr-name,valueonly)"})
  local pr_file = has_trailer and has_trailer ~= "" and path.join(".github/pr", has_trailer) or nil

  local msg: string
  if pr_file and path.exists(pr_file) then
    msg = string.format("PR file: %s - update if needed", pr_file)
  elseif has_trailer and has_trailer ~= "" then
    msg = string.format("PR file %s not found - create it", pr_file)
  else
    msg = "Consider adding x-cosmic-pr-name trailer and .github/pr/<name>.md"
  end

  return {
    hookSpecificOutput = {
      postToolUse = { additionalContext = msg }
    }
  }
end
register(post_commit_pr_reminder)

local function post_push_pr_check(input: HookInput): HookOutput, string
  if input.hook_event_name ~= "PostToolUse" then
    return nil
  end
  if input.tool_name ~= "Bash" then
    return nil
  end

  local tool_input = input.tool_input or {} as ToolInput
  local command = tool_input.command or ""
  if not command:match("git push") then
    return nil
  end

  local branch = spawn_capture({"git", "rev-parse", "--abbrev-ref", "HEAD"})
  if not branch or branch == "main" or branch == "master" then
    return nil
  end

  local trailer = spawn_capture({"git", "log", "-1", "--format=%(trailers:key=x-cosmic-pr-name,valueonly)"})
  local has_trailer = trailer and trailer ~= ""

  local msg: string
  if has_trailer then
    local pr_file = path.join(".github/pr", trailer)
    if path.exists(pr_file) then
      msg = string.format("Confirm %s accurately describes the changes. Revise if needed.", pr_file)
    else
      msg = string.format("Create %s to describe the PR changes.", pr_file)
    end
  else
    msg = "Write a PR description in .github/pr/<name>.md and add x-cosmic-pr-name: <name>.md trailer to the commit."
  end

  return {
    hookSpecificOutput = {
      postToolUse = { additionalContext = msg }
    }
  }
end
register(post_push_pr_check)

local function post_push_check_reminder(input: HookInput): HookOutput, string
  if input.hook_event_name ~= "PostToolUse" then
    return nil
  end
  if input.tool_name ~= "Bash" then
    return nil
  end

  local tool_input = input.tool_input or {} as ToolInput
  local command = tool_input.command or ""
  if not command:match("git push") then
    return nil
  end

  local branch = spawn_capture({"git", "rev-parse", "--abbrev-ref", "HEAD"})
  if not branch or branch == "main" or branch == "master" then
    return nil
  end

  return {
    hookSpecificOutput = {
      postToolUse = { additionalContext = "Remember to run checks on feature branches (see `make help`)" }
    }
  }
end
register(post_push_check_reminder)

--------------------------------------------------------------------------------
-- module
--------------------------------------------------------------------------------

if cosmo.is_main() then
  os.exit(main())
end

return {
  _VERSION = "0.1.0",
  _DESCRIPTION = "Claude Code hook dispatcher",
  register = register,
  read_input = read_input,
  write_output = write_output,
  dispatch = dispatch,
  run = run,
  main = main,
}
