-- box/env.tl: load env configuration from directory

local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local function parse_env_file(filepath: string): {string:string}
  local content = cosmo.Slurp(filepath)
  if not content then
    return {}
  end

  local env: {string:string} = {}
  for line in content:gmatch("[^\n]+") do
    local trimmed = line:match("^%s*(.-)%s*$")
    if trimmed ~= "" and not trimmed:match("^#") then
      local key, value = trimmed:match("^([^=]+)=(.*)$")
      if key and value then
        env[key] = value
      end
    end
  end
  return env
end

local function load_from_dir(dir: string): {string:string}
  local env: {string:string} = {}

  local entries, err = unix.opendir(dir)
  if not entries then
    return env
  end

  -- Sort entries alphabetically
  table.sort(entries, function(a, b): boolean
    return a < b
  end)

  -- Read files in alphabetical order
  for _, entry in ipairs(entries) do
    if entry ~= "." and entry ~= ".." then
      local filepath = path.join(dir, entry)
      local stat = unix.stat(filepath)
      if stat and stat.type == "file" then
        local file_env = parse_env_file(filepath)
        for k, v in pairs(file_env) do
          env[k] = v
        end
      end
    end
  end

  return env
end

local function load(): {string:string}
  local home = os.getenv("HOME")
  if not home then
    return {}
  end

  local env_dir = path.join(home, ".config", "box", "env.d")
  return load_from_dir(env_dir)
end

return {
  load = load,
  load_from_dir = load_from_dir,
  parse_env_file = parse_env_file,
}
