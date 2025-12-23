-- Configuration and path discovery for work system
local M = {}

M.defaults = {
  search_patterns = {
    "~/*/*/work",      -- matches ~/foo/progress/work
    "~/progress/work", -- direct path fallback
  },
}

-- Find work data directory using glob patterns
-- Returns: path, err
M.find_data_dir = function(patterns)
  patterns = patterns or M.defaults.search_patterns

  local posix_glob = require("posix.glob")
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
  local tried = {}
  for _, pattern in ipairs(patterns) do
    table.insert(tried, pattern)
  end
  return nil, "could not find work data directory, tried: " .. table.concat(tried, ", ")
end

-- Get configuration with optional overrides
-- Returns: config table
M.get = function(overrides)
  overrides = overrides or {}

  local config = {}

  -- Use explicit data_dir if provided
  if overrides.data_dir then
    config.data_dir = overrides.data_dir
  else
    local dir, err = M.find_data_dir(overrides.search_patterns)
    if not dir then
      error(err)
    end
    config.data_dir = dir
  end

  return config
end

return M
