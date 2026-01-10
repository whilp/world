-- hooks - manage claude code hook configuration
-- usage: cosmic -l skill hooks <command> [args...]

local cosmo = require("cosmo")
local path = require("cosmo.path")
local unix = require("cosmo.unix")

local M = {}

-- available handlers that can be enabled/disabled
M.available_handlers = {
  session_start_bootstrap = {
    description = "Add bin/ to PATH on session start",
    event = "SessionStart",
  },
  session_start_make_help = {
    description = "Display make help on session start",
    event = "SessionStart",
  },
  post_commit_pr_reminder = {
    description = "Remind about PR files after git commits",
    event = "PostToolUse",
  },
  stop_check_pr_file = {
    description = "Block stop unless PR file exists and is up-to-date",
    event = "Stop",
  },
  stop_check_reminder = {
    description = "Remind to run checks on feature branches",
    event = "Stop",
  },
}

local function log(msg)
  io.stderr:write("hooks: " .. msg .. "\n")
end

-- find the hooks config file path
local function get_config_path()
  local cwd = unix.getcwd()
  if not cwd then
    return nil, "failed to get cwd"
  end
  return path.join(cwd, ".claude", "hooks.lua")
end

-- load hooks config from file
function M.load_config(config_path)
  config_path = config_path or get_config_path()
  if not config_path then
    return nil, "failed to get config path"
  end

  if not path.exists(config_path) then
    -- return empty config if file doesn't exist
    return {}
  end

  local chunk, err = loadfile(config_path)
  if not chunk then
    return nil, "failed to load " .. config_path .. ": " .. tostring(err)
  end

  local ok, result = pcall(chunk)
  if not ok then
    return nil, "failed to execute " .. config_path .. ": " .. tostring(result)
  end

  if type(result) ~= "table" then
    return nil, "config must return a table"
  end

  return result
end

-- serialize a lua value to string
local function serialize_value(v, indent)
  indent = indent or ""
  local t = type(v)

  if t == "string" then
    return string.format("%q", v)
  elseif t == "number" or t == "boolean" then
    return tostring(v)
  elseif t == "table" then
    local parts = {}
    local next_indent = indent .. "  "

    -- check if table is array-like
    local is_array = true
    local max_index = 0
    for k, _ in pairs(v) do
      if type(k) ~= "number" or k < 1 or math.floor(k) ~= k then
        is_array = false
        break
      end
      if k > max_index then max_index = k end
    end
    is_array = is_array and max_index == #v

    if is_array then
      for i, val in ipairs(v) do
        parts[i] = next_indent .. serialize_value(val, next_indent)
      end
      if #parts == 0 then
        return "{}"
      end
      return "{\n" .. table.concat(parts, ",\n") .. ",\n" .. indent .. "}"
    else
      -- sort keys for consistent output
      local keys = {}
      for k in pairs(v) do
        keys[#keys + 1] = k
      end
      table.sort(keys, function(a, b)
        return tostring(a) < tostring(b)
      end)

      for _, k in ipairs(keys) do
        local key_str
        if type(k) == "string" and k:match("^[%a_][%w_]*$") then
          key_str = k
        else
          key_str = "[" .. serialize_value(k) .. "]"
        end
        parts[#parts + 1] = next_indent .. key_str .. " = " .. serialize_value(v[k], next_indent)
      end

      if #parts == 0 then
        return "{}"
      end
      return "{\n" .. table.concat(parts, ",\n") .. ",\n" .. indent .. "}"
    end
  else
    return "nil"
  end
end

-- save hooks config to file
function M.save_config(config, config_path)
  config_path = config_path or get_config_path()
  if not config_path then
    return nil, "failed to get config path"
  end

  -- ensure .claude directory exists
  local dir = path.dirname(config_path)
  if not path.exists(dir) then
    local ok, err = unix.makedirs(dir, tonumber("755", 8))
    if not ok then
      return nil, "failed to create directory " .. dir .. ": " .. tostring(err)
    end
  end

  local content = "-- hooks configuration\n"
  content = content .. "-- each handler can be:\n"
  content = content .. "--   true              -> enabled with defaults\n"
  content = content .. "--   false             -> disabled\n"
  content = content .. "--   { enabled = bool, ... }  -> enabled with config options\n"
  content = content .. "return " .. serialize_value(config) .. "\n"

  local f, err = io.open(config_path, "w")
  if not f then
    return nil, "failed to open " .. config_path .. ": " .. tostring(err)
  end

  local ok, write_err = f:write(content)
  f:close()

  if not ok then
    return nil, "failed to write " .. config_path .. ": " .. tostring(write_err)
  end

  return true
end

-- check if a handler is enabled in config
function M.is_enabled(config, handler_name)
  local entry = config[handler_name]
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

-- get config options for a handler
function M.get_handler_config(config, handler_name)
  local entry = config[handler_name]
  if type(entry) == "table" then
    return entry
  end
  return {}
end

-- command: show - display current configuration
local function cmd_show()
  local config, err = M.load_config()
  if not config then
    return 1, err
  end

  print("-- hooks configuration")
  print("return " .. serialize_value(config))
  return 0
end

-- command: list - list available handlers
local function cmd_list()
  local config, err = M.load_config()
  if not config then
    return 1, err
  end

  -- group by event
  local by_event = {}
  for name, info in pairs(M.available_handlers) do
    local event = info.event
    if not by_event[event] then
      by_event[event] = {}
    end
    by_event[event][#by_event[event] + 1] = {name = name, info = info}
  end

  -- sort events
  local events = {}
  for event in pairs(by_event) do
    events[#events + 1] = event
  end
  table.sort(events)

  for _, event in ipairs(events) do
    print(event .. ":")
    -- sort handlers within event
    local handlers = by_event[event]
    table.sort(handlers, function(a, b) return a.name < b.name end)

    for _, h in ipairs(handlers) do
      local status = M.is_enabled(config, h.name) and "[enabled]" or "[disabled]"
      print(string.format("  %s %s", status, h.name))
      print(string.format("    %s", h.info.description))
    end
  end

  return 0
end

-- command: add - enable a handler
local function cmd_add(handler_name, ...)
  if not handler_name then
    return 1, "usage: hooks add <handler> [key=value ...]"
  end

  if not M.available_handlers[handler_name] then
    return 1, "unknown handler: " .. handler_name .. "\nuse 'hooks list' to see available handlers"
  end

  local config, err = M.load_config()
  if not config then
    return 1, err
  end

  -- parse optional config options
  local opts = {...}
  if #opts > 0 then
    local handler_opts = {enabled = true}
    for _, opt in ipairs(opts) do
      local key, value = opt:match("^([^=]+)=(.+)$")
      if key and value then
        -- try to parse value as boolean or number
        if value == "true" then
          handler_opts[key] = true
        elseif value == "false" then
          handler_opts[key] = false
        elseif tonumber(value) then
          handler_opts[key] = tonumber(value)
        else
          handler_opts[key] = value
        end
      else
        return 1, "invalid option format: " .. opt .. " (expected key=value)"
      end
    end
    config[handler_name] = handler_opts
  else
    config[handler_name] = true
  end

  local ok
  ok, err = M.save_config(config)
  if not ok then
    return 1, err
  end

  log("enabled " .. handler_name)
  return 0
end

-- command: remove - disable a handler
local function cmd_remove(handler_name)
  if not handler_name then
    return 1, "usage: hooks remove <handler>"
  end

  local config, err = M.load_config()
  if not config then
    return 1, err
  end

  if config[handler_name] == nil then
    log(handler_name .. " is not configured")
    return 0
  end

  config[handler_name] = nil

  local ok
  ok, err = M.save_config(config)
  if not ok then
    return 1, err
  end

  log("removed " .. handler_name)
  return 0
end

-- command: disable - disable a handler but keep config
local function cmd_disable(handler_name)
  if not handler_name then
    return 1, "usage: hooks disable <handler>"
  end

  local config, err = M.load_config()
  if not config then
    return 1, err
  end

  local current = config[handler_name]
  if type(current) == "table" then
    current.enabled = false
    config[handler_name] = current
  else
    config[handler_name] = false
  end

  local ok
  ok, err = M.save_config(config)
  if not ok then
    return 1, err
  end

  log("disabled " .. handler_name)
  return 0
end

local function print_help()
  print([[
usage: hooks <command> [args...]

Manage Claude Code hook configuration stored in .claude/hooks.lua

Commands:
  show              Display current hooks configuration
  list              List available handlers with status
  add <handler>     Enable a handler (optionally with key=value config)
  remove <handler>  Remove a handler from configuration
  disable <handler> Disable a handler but keep its configuration

Examples:
  hooks show
  hooks list
  hooks add session_start_make_help
  hooks add stop_check_pr_file strict=true
  hooks disable post_commit_pr_reminder
  hooks remove stop_check_pr_file
]])
end

function M.main()
  local cmd = arg[1]

  if not cmd or cmd == "-h" or cmd == "--help" or cmd == "help" then
    print_help()
    return 0
  end

  if cmd == "show" then
    return cmd_show()
  elseif cmd == "list" then
    return cmd_list()
  elseif cmd == "add" then
    return cmd_add(arg[2], select(3, table.unpack(arg)))
  elseif cmd == "remove" then
    return cmd_remove(arg[2])
  elseif cmd == "disable" then
    return cmd_disable(arg[2])
  else
    return 1, "unknown command: " .. cmd .. "\nuse 'hooks help' for usage"
  end
end

return M
