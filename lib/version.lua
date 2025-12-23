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
  local ok, err = type_check(config.platforms, "table", "platforms")
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
    local ok, err = type_check(config.executables, "table", "executables")
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
  local loader, err = loadfile(path)
  if not loader then
    return false, err
  end

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

  setfenv(loader, wrapped_env)
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
  return template:gsub("%${([%w_]+)}", function(key)
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

return M
