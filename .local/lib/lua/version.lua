local M = {}

M.versions = {}

local function default_version_callback(config)
  if not config.name then
    error("version config missing 'name' field")
  end

  if M.versions[config.name] then
    error("version name collision: " .. config.name)
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
