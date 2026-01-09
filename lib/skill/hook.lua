-- hook - claude code hook dispatcher
-- receives hook events via stdin JSON, dispatches to registered handlers
-- usage: cosmic --skill hook

local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

-- list of handlers, each decides whether to handle based on input
-- handler signature: function(input) -> output_table|nil, error_string|nil
local handlers = {}

local function register(handler)
  handlers[#handlers + 1] = handler
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

local function dispatch(input)
  local outputs = {}
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

  local output
  output, err = dispatch(input)
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

local function session_start_bootstrap(input)
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
}
