-- Configuration and path discovery for work system

local posix_glob = require("posix.glob")

-- Record type for configuration defaults
local record Defaults
  search_patterns: {string}
end

-- Record type for configuration
local record Config
  data_dir: string
end

-- Record type for configuration overrides
local record ConfigOverrides
  data_dir: string
  search_patterns: {string}
end

local record M
  defaults: Defaults
  find_data_dir: function(patterns?: {string}): string, string
  get: function(overrides?: ConfigOverrides): Config
end

local config: M = {}

config.defaults = {
  search_patterns = {
    "~/*/*/work",      -- matches ~/foo/progress/work
    "~/progress/work", -- direct path fallback
  },
}

-- Find work data directory using glob patterns
-- Returns: path, err
config.find_data_dir = function(patterns?: {string}): string, string
  patterns = patterns or config.defaults.search_patterns

  local home = os.getenv("HOME")

  for _, pattern in ipairs(patterns) do
    -- Expand ~ to home directory
    local expanded = pattern:gsub("^~", home)

    local matches = posix_glob.glob(expanded, 0)
    if matches and #matches > 0 then
      -- Return first match
      return matches[1]
    end
  end

  -- If nothing found, return error with attempted patterns
  local tried: {string} = {}
  for _, pattern in ipairs(patterns) do
    table.insert(tried, pattern)
  end
  return nil, "could not find work data directory, tried: " .. table.concat(tried, ", ")
end

-- Get configuration with optional overrides
-- Returns: config table
config.get = function(overrides?: ConfigOverrides): Config
  overrides = overrides or {} as ConfigOverrides

  local cfg: Config = {} as Config

  -- Use explicit data_dir if provided
  if overrides.data_dir then
    cfg.data_dir = overrides.data_dir
  else
    local dir, err = config.find_data_dir(overrides.search_patterns)
    if not dir then
      error(err)
    end
    cfg.data_dir = dir
  end

  return cfg
end

return config
