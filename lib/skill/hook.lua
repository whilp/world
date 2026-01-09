-- hook - claude code hook dispatcher
-- receives hook events via stdin JSON, dispatches to registered handlers
-- usage: cosmic --skill hook

local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

-- list of handlers, each decides whether to handle based on input
-- handler signature: function(input, config) -> output_table|nil, error_string|nil
local handlers = {}

-- handler registry with names for config lookup
local handler_registry = {}

local function register(handler, name)
  handlers[#handlers + 1] = handler
  if name then
    handler_registry[name] = handler
  end
end

-- load hooks configuration
local function load_hooks_config()
  local cwd = unix.getcwd()
  if not cwd then
    return {}
  end

  local config_path = path.join(cwd, ".claude", "hooks.lua")
  if not path.exists(config_path) then
    -- return config with all handlers enabled by default
    return {
      session_start_bootstrap = true,
      session_start_make_help = true,
      post_commit_pr_reminder = true,
      stop_check_pr_file = true,
    }
  end

  local chunk, err = loadfile(config_path)
  if not chunk then
    io.stderr:write("hook: failed to load config: " .. tostring(err) .. "\n")
    return {}
  end

  local ok, result = pcall(chunk)
  if not ok then
    io.stderr:write("hook: failed to execute config: " .. tostring(result) .. "\n")
    return {}
  end

  if type(result) ~= "table" then
    return {}
  end

  return result
end

-- check if handler is enabled in config
local function is_handler_enabled(config, name)
  local entry = config[name]
  if entry == nil then
    return false
  end
  if type(entry) == "boolean" then
    return entry
  end
  if type(entry) == "table" then
    return entry.enabled ~= false
  end
  return false
end

-- get handler config options
local function get_handler_config(config, name)
  local entry = config[name]
  if type(entry) == "table" then
    return entry
  end
  return {}
end

local function read_input(stdin)
  stdin = stdin or io.stdin
  local raw = stdin:read("*a")
  if not raw or raw == "" then
    return nil, "no input"
  end
  local input = cosmo.DecodeJson(raw)
  if not input then
    return nil, "invalid json"
  end
  return input
end

local function write_output(output, stdout)
  stdout = stdout or io.stdout
  if output then
    stdout:write(cosmo.EncodeJson(output))
  end
end

local function dispatch(input, config)
  config = config or {}
  local outputs = {}

  for name, handler in pairs(handler_registry) do
    if is_handler_enabled(config, name) then
      local handler_config = get_handler_config(config, name)
      local output, err = handler(input, handler_config)
      if err then
        return nil, err
      end
      if output then
        outputs[#outputs + 1] = output
      end
    end
  end

  if #outputs == 0 then
    return nil
  end

  -- merge outputs (last handler wins for conflicts)
  local merged = {}
  for _, out in ipairs(outputs) do
    for k, v in pairs(out) do
      merged[k] = v
    end
  end
  return merged
end

local function run(opts)
  opts = opts or {}
  local input, err = read_input(opts.stdin)
  if not input then
    return 0
  end

  local config = opts.config or load_hooks_config()

  local output
  output, err = dispatch(input, config)
  if err then
    io.stderr:write("hook: " .. err .. "\n")
    return 2, err
  end

  write_output(output, opts.stdout)
  return 0
end

local function main()
  return run()
end

--------------------------------------------------------------------------------
-- built-in handlers
--------------------------------------------------------------------------------

local function spawn_capture(cmd)
  local spawn = require("cosmic.spawn").spawn
  local handle = spawn(cmd)
  local ok, out = handle:read()
  if not ok then
    return nil
  end
  return out and out:match("^%s*(.-)%s*$")
end

local function session_start_bootstrap(input, handler_config)
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
register(session_start_bootstrap, "session_start_bootstrap")

local function session_start_make_help(input, handler_config)
  if input.hook_event_name ~= "SessionStart" then
    return nil
  end
  if input.source and input.source ~= "startup" then
    return nil
  end

  local help = spawn_capture({"make", "help"})
  if not help or help == "" then
    return nil
  end

  io.stdout:write(help .. "\n")
  return nil
end
register(session_start_make_help, "session_start_make_help")

local function post_commit_pr_reminder(input, handler_config)
  if input.hook_event_name ~= "PostToolUse" then
    return nil
  end
  if input.tool_name ~= "Bash" then
    return nil
  end

  local tool_input = input.tool_input or {}
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

  local msg
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
register(post_commit_pr_reminder, "post_commit_pr_reminder")

local function stop_check_pr_file(input, handler_config)
  if input.hook_event_name ~= "Stop" then
    return nil
  end

  local branch = spawn_capture({"git", "rev-parse", "--abbrev-ref", "HEAD"})
  if not branch or branch == "main" or branch == "master" then
    return nil
  end

  local trailer = spawn_capture({"git", "log", "-1", "--format=%(trailers:key=x-cosmic-pr-name,valueonly)"})
  if not trailer or trailer == "" then
    return nil
  end

  local pr_file = path.join(".github/pr", trailer)
  if not path.exists(pr_file) then
    return {
      decision = "block",
      reason = string.format("PR file %s not found - create it before finishing", pr_file),
    }
  end

  -- check if PR file is older than latest commit (unless strict=false)
  if handler_config.strict == false then
    return nil
  end

  local pr_stat = unix.stat(pr_file)
  local commit_time = spawn_capture({"git", "log", "-1", "--format=%ct"})
  if pr_stat and commit_time then
    local pr_mtime = pr_stat:mtim()
    local commit_ts = tonumber(commit_time) or 0
    if commit_ts > pr_mtime then
      return {
        decision = "block",
        reason = string.format("PR file %s is older than latest commit - update it", pr_file),
      }
    end
  end

  return nil
end
register(stop_check_pr_file, "stop_check_pr_file")

--------------------------------------------------------------------------------
-- module
--------------------------------------------------------------------------------

return {
  _VERSION = "0.1.0",
  _DESCRIPTION = "Claude Code hook dispatcher",
  register = register,
  read_input = read_input,
  write_output = write_output,
  dispatch = dispatch,
  run = run,
  main = main,
  load_hooks_config = load_hooks_config,
  is_handler_enabled = is_handler_enabled,
  get_handler_config = get_handler_config,
  handler_registry = handler_registry,
}
