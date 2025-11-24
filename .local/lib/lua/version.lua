local M = {}

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
