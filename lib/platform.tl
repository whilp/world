local version = require("version")

local record PlatformData
  sha256: string
  arch: string
  ext: string
end

local record PlatformConfig
  name: string
  version: string
  url: string
  path: string
  repo: string
  strip_components: number
  platforms: {string:PlatformData}
  urls: {string:string}
  platform: string
end

local SYSTEM_MAP <const>: {string:string} = {
  osx = "darwin",
}

local ARCH_MAP <const>: {string:string} = {
  x64 = "x86_64",
  aarch64 = "arm64",
  arm64 = "arm64",
}

local function normalize_system(system: string): string
  return SYSTEM_MAP[system] or system
end

local function normalize_arch(arch: string): string
  return ARCH_MAP[arch] or arch
end

local function detect(system: string, arch: string): string, string
  if not system or not arch then
    return nil, "unable to detect platform (system and arch must be provided)"
  end

  system = normalize_system(system)
  arch = normalize_arch(arch)

  return system .. "-" .. arch
end

local function build_context(config: PlatformConfig, platform_data: PlatformData): {string:any}
  local ctx: {string:any} = {}
  for k, v in pairs(config as {string:any}) do
    if k ~= "platforms" and k ~= "urls" and not (k as string):match("^_") then
      ctx[k] = v
    end
  end
  return version.merge(ctx, platform_data as {string:any} or {})
end

local function validate_platform_config(config: PlatformConfig, platform: string): boolean, string
  if not config.platforms then
    return nil, "config missing 'platforms' field"
  end

  if not config.platforms[platform] then
    return nil, "no configuration for platform: " .. platform
  end

  return true
end

local function get_platform_config(config: PlatformConfig, platform: string): PlatformConfig, string
  platform = platform or detect(nil, nil)
  if not platform then
    return nil, "unable to detect platform"
  end

  local ok, err = validate_platform_config(config, platform)
  if not ok then
    return nil, err
  end

  local platform_data = config.platforms[platform]

  local merged: PlatformConfig = {} as PlatformConfig
  for k, v in pairs(config as {string:any}) do
    if k ~= "platforms" and k ~= "urls" and k ~= "url" then
      (merged as {string:any})[k] = v
    end
  end
  for k, v in pairs(platform_data as {string:any}) do
    (merged as {string:any})[k] = v
  end
  merged.platform = platform

  if config.urls and config.urls[platform] then
    (merged as {string:any})["url"] = config.urls[platform]
  end

  return merged
end

local function expand(config: PlatformConfig): PlatformConfig
  local base_context = build_context(config, nil)

  for k, v in pairs(config as {string:any}) do
    if k ~= "platforms" and k ~= "urls" and k ~= "url" and not (k as string):match("^_") then
      (config as {string:any})[k] = version.expand(v, base_context)
    end
  end

  local expanded_platforms: {string:PlatformData} = {}
  for plat, platform_data in pairs(config.platforms or {}) do
    local context = build_context(config, platform_data)
    context["platform"] = plat
    expanded_platforms[plat] = version.expand(platform_data, context) as PlatformData
  end
  config.platforms = expanded_platforms

  if (config as {string:any})["url"] then
    config.urls = {}
    for plat, platform_data in pairs(config.platforms) do
      local context = build_context(config, platform_data)
      context["platform"] = plat
      config.urls[plat] = version.interpolate((config as {string:any})["url"] as string, context)
    end
  end

  return config
end

local function get_field(config: PlatformConfig, field_path: string, platform: string): any, string
  platform = platform or detect(nil, nil)

  if field_path == "url" then
    if not config.urls then
      return nil, "config has no 'urls' field"
    end
    if not config.urls[platform] then
      return nil, "no URL for platform: " .. platform
    end
    return config.urls[platform]
  end

  local keys: {string} = {}
  for key in field_path:gmatch("[^.]+") do
    table.insert(keys, key)
  end

  if config.platforms and config.platforms[platform] then
    local current: any = config.platforms[platform]
    local found = true
    for _, key in ipairs(keys) do
      if type(current) ~= "table" then
        found = false
        break
      end
      current = (current as {string:any})[key]
      if current == nil then
        found = false
        break
      end
    end
    if found then
      return current
    end
  end

  local current: any = config
  for _, key in ipairs(keys) do
    if type(current) ~= "table" then
      return nil, "not a table at key: " .. key
    end
    current = (current as {string:any})[key]
    if current == nil then
      return nil, "field not found: " .. field_path
    end
  end

  return current
end

local function get(name: string, platform: string): PlatformConfig, string
  local config = version.get(name)
  if not config then
    return nil, "config not found: " .. name
  end

  if platform then
    return get_platform_config(config as PlatformConfig, platform)
  end

  return config as PlatformConfig
end

return {
  normalize_system = normalize_system,
  normalize_arch = normalize_arch,
  detect = detect,
  get_platform_config = get_platform_config,
  get_config = get_platform_config,
  expand = expand,
  get_field = get_field,
  load_file = version.load_file,
  get = get,
  versions = version.versions,
  write = version.write,
  render = version.render,
  get_by_file = version.get_by_file,
}
