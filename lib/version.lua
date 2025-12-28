local M = {}

M.versions = {}

local function validate_config(config)
  local function type_check(value, expected_type, field_name)
    local actual_type = type(value)
    if actual_type ~= expected_type then
      return nil, string.format("field '%s' must be %s, got %s", field_name, expected_type, actual_type)
    end
    return true
  end

  if not config.name then
    return nil, "missing required field 'name'"
  end
  local ok, err = type_check(config.name, "string", "name")
  if not ok then return nil, err end

  if not config.platforms then
    return nil, "missing required field 'platforms'"
  end
  ok, err = type_check(config.platforms, "table", "platforms")
  if not ok then return nil, err end

  local platform_count = 0
  for plat, platform_data in pairs(config.platforms) do
    platform_count = platform_count + 1

    if type(platform_data) ~= "table" then
      return nil, string.format("platform '%s' must be a table, got %s", plat, type(platform_data))
    end

    if not platform_data.sha256 then
      return nil, string.format("platform '%s' missing required field 'sha256'", plat)
    end

    if type(platform_data.sha256) ~= "string" then
      return nil, string.format("platform '%s' field 'sha256' must be string, got %s", plat, type(platform_data.sha256))
    end

    if #platform_data.sha256 ~= 64 then
      return nil, string.format(
        "platform '%s' field 'sha256' must be 64 characters (SHA-256 hex), got %d",
        plat, #platform_data.sha256)
    end

    if platform_data.arch and type(platform_data.arch) ~= "string" then
      return nil, string.format("platform '%s' field 'arch' must be string, got %s", plat, type(platform_data.arch))
    end

    if platform_data.ext and type(platform_data.ext) ~= "string" then
      return nil, string.format("platform '%s' field 'ext' must be string, got %s", plat, type(platform_data.ext))
    end
  end

  if platform_count == 0 then
    return nil, "platforms table cannot be empty, must have at least one platform"
  end

  if config.path and type(config.path) ~= "string" then
    return nil, string.format("field 'path' must be string, got %s", type(config.path))
  end

  if config.url and type(config.url) ~= "string" then
    return nil, string.format("field 'url' must be string, got %s", type(config.url))
  end

  if config.version and type(config.version) ~= "string" then
    return nil, string.format("field 'version' must be string, got %s", type(config.version))
  end

  if config.repo and type(config.repo) ~= "string" then
    return nil, string.format("field 'repo' must be string, got %s", type(config.repo))
  end

  if config.strip_components then
    if type(config.strip_components) ~= "number" then
      return nil, string.format("field 'strip_components' must be number, got %s", type(config.strip_components))
    end
    if config.strip_components < 0 or config.strip_components ~= math.floor(config.strip_components) then
      return nil, string.format(
        "field 'strip_components' must be non-negative integer, got %s",
        tostring(config.strip_components))
    end
  end

  if config.executables then
    ok, err = type_check(config.executables, "table", "executables")
    if not ok then return nil, err end

    for i, exe in ipairs(config.executables) do
      if type(exe) ~= "table" then
        return nil, string.format("executables[%d] must be table, got %s", i, type(exe))
      end

      if not exe.name then
        return nil, string.format("executables[%d] missing required field 'name'", i)
      end

      if type(exe.name) ~= "string" then
        return nil, string.format("executables[%d] field 'name' must be string, got %s", i, type(exe.name))
      end

      if not exe.path then
        return nil, string.format("executables[%d] missing required field 'path'", i)
      end

      if type(exe.path) ~= "string" then
        return nil, string.format("executables[%d] field 'path' must be string, got %s", i, type(exe.path))
      end

      if exe.symlinks then
        if type(exe.symlinks) ~= "table" then
          return nil, string.format("executables[%d] field 'symlinks' must be table, got %s", i, type(exe.symlinks))
        end
        for src, dst in pairs(exe.symlinks) do
          if type(src) ~= "string" or type(dst) ~= "string" then
            return nil, string.format("executables[%d] symlinks must map string to string", i)
          end
        end
      end
    end
  end

  return true
end

local function default_version_callback(config)
  local source = config._meta and config._meta.source or "unknown"

  local ok, err = validate_config(config)
  if not ok then
    error("config validation failed in " .. source .. ": " .. err)
  end

  if M.versions[config.name] then
    local existing = M.versions[config.name]
    local existing_path = existing._meta and existing._meta.source or "unknown"
    error("version name collision: " .. config.name .. " (existing: " .. existing_path .. ", new: " .. source .. ")")
  end

  M.versions[config.name] = config
end

M.load_file = function(path, kinds)
  local env = kinds or {}

  if not env.Version then
    env.Version = default_version_callback
  end

  local wrapped_env = {}
  for kind_name, callback in pairs(env) do
    wrapped_env[kind_name] = function(config)
      if not config._meta then
        config._meta = {}
      end
      config._meta.source = debug.getinfo(2, "S").source:sub(2)
      config._meta.kind = kind_name
      callback(config)
    end
  end

  local loader, err = loadfile(path, "t", wrapped_env)
  if not loader then
    return false, err
  end

  local ok, load_err = pcall(loader)

  return ok, load_err
end

M.write = function(config, callback_name)
  callback_name = callback_name or config._meta.kind or "Version"

  local clean = {}
  for k, v in pairs(config) do
    if not k:match("^_") then
      clean[k] = v
    end
  end

  local rendered = M.render(clean, {exclude = {"urls"}})
  rendered = rendered:gsub("^return ", callback_name)
  return rendered
end

M.get = function(name)
  return M.versions[name]
end

M.get_by_file = function(source)
  local configs = {}
  for _, config in pairs(M.versions) do
    if config._meta and config._meta.source == source then
      table.insert(configs, config)
    end
  end
  return configs
end

function M.interpolate(template, context)
  if type(template) ~= "string" then
    return template
  end
  return template:gsub("{([%w_]+)}", function(key)
    return tostring(context[key] or "")
  end)
end

function M.expand(value, context)
  if type(value) == "string" then
    return M.interpolate(value, context)
  elseif type(value) == "table" then
    local expanded = {}
    for k, v in pairs(value) do
      expanded[k] = M.expand(v, context)
    end
    return expanded
  else
    return value
  end
end

function M.merge(...)
  local result = {}
  for _, tbl in ipairs({...}) do
    if type(tbl) == "table" then
      for k, v in pairs(tbl) do
        result[k] = v
      end
    end
  end
  return result
end

function M.load(source)
  if source:match("\n") or source:match("^return ") then
    local loader, err = load(source)
    if not loader then
      error("failed to load: " .. err)
    end
    return loader()
  else
    local loader, err = loadfile(source)
    if not loader then
      error("failed to load: " .. err)
    end
    return loader()
  end
end

function M.render(data, opts)
  opts = opts or {}
  local format = opts.format or "serpent"
  local exclude = opts.exclude or {}

  local filtered = {}
  for k, v in pairs(data) do
    local skip = false
    for _, excl in ipairs(exclude) do
      if k == excl then
        skip = true
        break
      end
    end
    if not skip then
      filtered[k] = v
    end
  end

  if format == "serpent" then
    local serpent = require("serpent")
    return "return " .. serpent.block(filtered, {
      comment = false,
      sortkeys = true,
      indent = opts.indent or "  ",
    }) .. "\n"
  else
    return tostring(filtered)
  end
end

local cosmo = require("cosmo")
local unix = cosmo.unix
local path = cosmo.path

local HOME = os.getenv("HOME")
local DEFAULT_SHARE_DIR = path.join(HOME, ".local", "share")

function M.parse_version_dir(name)
  local version, sha = name:match("^(.+)%-(%x+)$")
  if version and sha then
    return version, sha
  end
  return nil, nil
end

function M.compare_versions(a, b)
  local function parse_components(v)
    local parts = {}
    for part in v:gmatch("[^.-]+") do
      local num = tonumber(part)
      table.insert(parts, num or part)
    end
    return parts
  end

  local pa, pb = parse_components(a), parse_components(b)
  local max_len = math.max(#pa, #pb)

  for i = 1, max_len do
    local va, vb = pa[i], pb[i]
    if va == nil then return false end
    if vb == nil then return true end

    local ta, tb = type(va), type(vb)
    if ta == "number" and tb == "number" then
      if va ~= vb then return va > vb end
    elseif ta == "number" then
      return true
    elseif tb == "number" then
      return false
    else
      if va ~= vb then return va > vb end
    end
  end

  return false
end

function M.find_binary(version_dir, tool_name)
  local binary_path = path.join(version_dir, "bin", tool_name)
  local st = unix.stat(binary_path)
  if st then
    local mode = type(st.mode) == "function" and st:mode() or st.mode
    if not unix.S_ISDIR(mode) then
      return binary_path
    end
  end
  return nil
end

function M.scan_versions(tool_name, share_dir)
  share_dir = share_dir or DEFAULT_SHARE_DIR
  local tool_dir = path.join(share_dir, tool_name)

  local st = unix.stat(tool_dir)
  if not st then
    return {}
  end
  local mode = type(st.mode) == "function" and st:mode() or st.mode
  if not unix.S_ISDIR(mode) then
    return {}
  end

  local versions = {}
  local dir = unix.opendir(tool_dir)
  if not dir then
    return versions
  end

  for name in dir do
    if name ~= "." and name ~= ".." then
      local version, sha = M.parse_version_dir(name)
      if version and sha then
        local version_path = path.join(tool_dir, name)
        local bin_path = M.find_binary(version_path, tool_name)
        if bin_path then
          table.insert(versions, {
            version = version,
            sha = sha,
            path = bin_path,
            dir = version_path,
          })
        end
      end
    end
  end

  table.sort(versions, function(a, b)
    return M.compare_versions(a.version, b.version)
  end)

  return versions
end

function M.find_latest(tool_name, share_dir)
  local versions = M.scan_versions(tool_name, share_dir)
  if #versions > 0 then
    return versions[1]
  end
  return nil
end

function M.resolve_bin(tool_name, share_dir)
  local latest = M.find_latest(tool_name, share_dir)
  if latest then
    return latest.path
  end
  return nil
end

return M
