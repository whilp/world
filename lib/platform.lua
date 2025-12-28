local version = require("version")
local M = {}

-- Platform detection
local SYSTEM_MAP = {
  osx = "darwin",
}

local ARCH_MAP = {
  x64 = "x86_64",
  aarch64 = "arm64",
  arm64 = "arm64",
}

function M.normalize_system(system)
  return SYSTEM_MAP[system] or system
end

function M.normalize_arch(arch)
  return ARCH_MAP[arch] or arch
end

function M.detect(system, arch)
  if not system or not arch then
    return nil, "unable to detect platform (system and arch must be provided)"
  end

  system = M.normalize_system(system)
  arch = M.normalize_arch(arch)

  return system .. "-" .. arch
end

-- Internal: build context for template expansion
local function build_context(config, platform_data)
  local ctx = {}
  for k, v in pairs(config) do
    if k ~= "platforms" and k ~= "urls" and not k:match("^_") then
      ctx[k] = v
    end
  end
  return version.merge(ctx, platform_data or {})
end

-- Validate platform config
local function validate_platform_config(config, platform)
  if not config.platforms then
    return nil, "config missing 'platforms' field"
  end

  if not config.platforms[platform] then
    return nil, "no configuration for platform: " .. platform
  end

  return true
end

-- Get platform-specific merged config from an expanded config
-- This should be called AFTER platform.expand() to get a single platform's view
function M.get_platform_config(config, platform)
  platform = platform or M.detect()
  if not platform then
    return nil, "unable to detect platform"
  end

  local ok, err = validate_platform_config(config, platform)
  if not ok then
    return nil, err
  end

  local platform_data = config.platforms[platform]

  local merged = {}
  for k, v in pairs(config) do
    if k ~= "platforms" and k ~= "urls" and k ~= "url" then
      merged[k] = v
    end
  end
  for k, v in pairs(platform_data) do
    merged[k] = v
  end
  merged.platform = platform

  if config.urls and config.urls[platform] then
    merged.url = config.urls[platform]
  end

  return merged
end

-- Deprecated: use get_platform_config instead
M.get_config = M.get_platform_config

-- Expand all template variables in config for all platforms
-- This performs a single expansion pass and generates the urls table
function M.expand(config)
  -- First pass: expand base fields with base context
  local base_context = build_context(config)

  for k, v in pairs(config) do
    if k ~= "platforms" and k ~= "urls" and k ~= "url" and not k:match("^_") then
      config[k] = version.expand(v, base_context)
    end
  end

  -- Second pass: expand each platform's config with merged context
  local expanded_platforms = {}
  for plat, platform_data in pairs(config.platforms or {}) do
    local context = build_context(config, platform_data)
    context.platform = plat
    expanded_platforms[plat] = version.expand(platform_data, context)
  end
  config.platforms = expanded_platforms

  -- Third pass: generate urls from url template
  if config.url then
    config.urls = {}
    for plat, platform_data in pairs(config.platforms) do
      local context = build_context(config, platform_data)
      context.platform = plat
      config.urls[plat] = version.interpolate(config.url, context)
    end
  end

  return config
end

-- Get field with platform awareness from an expanded config
-- First checks platform-specific fields, then falls back to base fields
function M.get_field(config, field_path, platform)
  platform = platform or M.detect()

  if field_path == "url" then
    if not config.urls then
      return nil, "config has no 'urls' field"
    end
    if not config.urls[platform] then
      return nil, "no URL for platform: " .. platform
    end
    return config.urls[platform]
  end

  local keys = {}
  for key in field_path:gmatch("[^.]+") do
    table.insert(keys, key)
  end

  -- First try platform-specific field
  if config.platforms and config.platforms[platform] then
    local current = config.platforms[platform]
    local found = true
    for _, key in ipairs(keys) do
      if type(current) ~= "table" then
        found = false
        break
      end
      current = current[key]
      if current == nil then
        found = false
        break
      end
    end
    if found then
      return current
    end
  end

  -- Fall back to base field
  local current = config
  for _, key in ipairs(keys) do
    if type(current) ~= "table" then
      return nil, "not a table at key: " .. key
    end
    current = current[key]
    if current == nil then
      return nil, "field not found: " .. field_path
    end
  end

  return current
end

-- Wrapper: load_file delegates to version
M.load_file = version.load_file

-- Wrapper: get with platform support
function M.get(name, platform)
  local config = version.get(name)
  if not config then
    return nil, "config not found: " .. name
  end

  if platform then
    return M.get_config(config, platform)
  end

  return config
end

-- Delegate other operations
M.versions = version.versions
M.write = version.write
M.render = version.render
M.get_by_file = version.get_by_file

return M
