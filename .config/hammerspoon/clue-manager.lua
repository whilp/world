local M = {}

M.active_modals = {}
M.canvas = nil

M.setup = function(loader)
  for prefix, modal_config in pairs(loader.modals) do
    M.create_modal(prefix, modal_config, loader)
  end
end

M.create_modal = function(prefix, config, loader)
  local trigger = config.trigger

  if #trigger < 2 then
    error("modal trigger must have at least 2 elements: " .. prefix)
  end

  local modifier = trigger[1]
  local key = trigger[2]

  local modal = hs.hotkey.modal.new(modifier, key)

  function modal:entered()
    M.show_overlay(config, loader, prefix)
  end

  function modal:exited()
    M.hide_overlay()
  end

  for _, binding in ipairs(config.bindings) do
    if not binding.key then
      error("binding missing key in modal: " .. prefix)
    end
    if not binding.clue then
      error("binding missing clue in modal: " .. prefix)
    end

    modal:bind("", binding.key, function()
      modal:exit()
      M.execute_clue(binding.clue.id, loader)
    end)
  end

  modal:bind("", "escape", function()
    modal:exit()
  end)

  M.active_modals[prefix] = modal
end

M.show_overlay = function(modal_config, loader, prefix)
  local screen = hs.mouse.getCurrentScreen()
  if not screen then
    return nil, "no screen found"
  end

  local screen_frame = screen:frame()

  local modal_name = "Commands"
  if #modal_config.bindings > 0 then
    local first_clue = modal_config.bindings[1].clue
    modal_name = first_clue.group or prefix
    modal_name = modal_name:gsub("^%l", string.upper)
  end

  local lines = { modal_name, "" }

  local sorted_bindings = {}
  for _, binding in ipairs(modal_config.bindings) do
    table.insert(sorted_bindings, binding)
  end
  table.sort(sorted_bindings, function(a, b)
    return a.key < b.key
  end)

  for _, binding in ipairs(sorted_bindings) do
    local clue = binding.clue
    local display_name = clue.name or clue.id or "unknown"
    table.insert(lines, string.format("  %s  →  %s", binding.key, display_name))
  end

  table.insert(lines, "")
  table.insert(lines, "  esc  →  Cancel")

  local text = table.concat(lines, "\n")

  local line_height = 20
  local canvas_height = #lines * line_height + 40
  local canvas_width = 400

  M.canvas = hs.canvas.new({
    x = screen_frame.x + screen_frame.w / 2 - canvas_width / 2,
    y = screen_frame.y + screen_frame.h - canvas_height - 50,
    w = canvas_width,
    h = canvas_height
  })

  M.canvas[1] = {
    type = "rectangle",
    action = "fill",
    fillColor = { alpha = 0.9, white = 0.1 },
    roundedRectRadii = { xRadius = 10, yRadius = 10 }
  }

  M.canvas[2] = {
    type = "text",
    text = text,
    textColor = { white = 1.0 },
    textSize = 14,
    textFont = "SF Mono",
  }

  M.canvas:show()
  return true
end

M.hide_overlay = function()
  if M.canvas then
    M.canvas:hide()
    M.canvas = nil
  end
end

M.execute_clue = function(clue_id, loader)
  if not clue_id then
    return nil, "clue_id cannot be nil"
  end

  local clue = loader.get_clue(clue_id)
  if not clue then
    hs.alert("Unknown clue: " .. tostring(clue_id))
    return nil, "unknown clue: " .. clue_id
  end

  local action = clue.action
  if not action then
    hs.alert("Clue missing action: " .. clue_id)
    return nil, "clue missing action: " .. clue_id
  end

  if action.url then
    local ok, err = hs.execute("open '" .. action.url .. "'")
    if not ok then
      hs.alert("Failed to open URL")
      return nil, "failed to open url: " .. err
    end
    return true
  elseif action.fn then
    if type(action.fn) ~= "function" then
      return nil, "action.fn must be a function"
    end
    local ok, result = pcall(action.fn)
    if not ok then
      hs.alert("Function execution failed")
      return nil, "function execution failed: " .. result
    end
    return true
  elseif action.shell then
    local ok, err = hs.execute(action.shell)
    if not ok then
      hs.alert("Shell command failed")
      return nil, "shell command failed: " .. err
    end
    return true
  elseif action.focus then
    local ok = hs.application.launchOrFocus(action.focus)
    if not ok then
      hs.alert("Failed to focus application: " .. action.focus)
      return nil, "failed to focus application: " .. action.focus
    end
    return true
  elseif action.mode then
    return action.mode
  else
    hs.alert("Unknown action type for clue: " .. clue_id)
    return nil, "unknown action type"
  end
end

return M
