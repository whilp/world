local M = {}

M.clues = {}
M.modals = {}
M.groups = {}

M.get_clue_files = function()
  local dir = hs.configdir .. "/clues"
  local files = {}

  local iter, dir_obj = hs.fs.dir(dir)
  if not iter then
    return files
  end

  for file in iter, dir_obj do
    if file:match("%.lua$") and file ~= "." and file ~= ".." then
      table.insert(files, dir .. "/" .. file)
    end
  end

  return files
end

local function generate_id(name)
  return name:lower():gsub("%s+", "-"):gsub("[^%w-]", "")
end

M.register_clue = function(config)
  if not config.name then
    error("clue must have name")
  end
  if not config.key then
    error("clue must have key")
  end
  if not config.action then
    error("clue must have action")
  end

  config.id = config.id or generate_id(config.name)

  if M.clues[config.id] then
    error("clue id collision: " .. config.id)
  end

  M.clues[config.id] = config

  if config.group then
    M.groups[config.group] = M.groups[config.group] or {}
    table.insert(M.groups[config.group], config)
  end
end

M.load_file = function(path)
  local env = {
    Clue = M.register_clue,
  }
  setmetatable(env, { __index = _G })

  local chunk, err = loadfile(path, "t", env)
  if not chunk then
    return nil, "failed to load file: " .. err
  end

  local ok, exec_err = pcall(chunk)
  if not ok then
    return nil, "failed to execute file: " .. exec_err
  end

  return true
end

M.generate_modals = function()
  local modal_groups = {}

  for id, clue in pairs(M.clues) do
    local key = clue.key

    if #key >= 3 then
      local prefix_parts = {}
      for i = 1, #key - 1 do
        table.insert(prefix_parts, key[i])
      end
      local prefix = table.concat(prefix_parts, ":")
      local final_key = key[#key]

      modal_groups[prefix] = modal_groups[prefix] or { clues = {} }
      table.insert(modal_groups[prefix].clues, {
        key = final_key,
        clue = clue,
      })
    end
  end

  for prefix, group in pairs(modal_groups) do
    local keys = {}
    for part in prefix:gmatch("[^:]+") do
      table.insert(keys, part)
    end

    M.modals[prefix] = {
      trigger = keys,
      bindings = group.clues,
    }
  end
end

M.load_all = function()
  local files = M.get_clue_files()

  for _, path in ipairs(files) do
    local ok, err = M.load_file(path)
    if not ok then
      io.stderr:write("error loading " .. path .. ": " .. err .. "\n")
      io.stderr:flush()
    end
  end

  M.generate_modals()

  return true
end

M.get_clue = function(id)
  return M.clues[id]
end

M.get_modal = function(prefix)
  return M.modals[prefix]
end

M.get_clues_for_group = function(group)
  return M.groups[group] or {}
end

M.to_choices = function()
  local choices = {}
  for id, clue in pairs(M.clues) do
    if clue.show_in_chooser ~= false then
      table.insert(choices, {
        text = clue.name,
        subText = clue.desc or clue.group or "",
        commandId = id,
      })
    end
  end
  return choices
end

return M
