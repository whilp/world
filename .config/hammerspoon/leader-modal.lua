local M = {}

M.config = {
  timeout_ms = 3000,
  overlay_columns = 3,
}

M.modal_stack = {}
M.timeout_timer = nil
M.canvas = nil
M.modals = {}
M.tree = {}
M.mode_handlers = {}

function M.setup(tree, config, mode_handlers)
  M.tree = tree or {}

  if config then
    for k, v in pairs(config) do
      M.config[k] = v
    end
  end

  M.mode_handlers = mode_handlers or {}

  hs.hotkey.bind({}, "f18", function()
    if #M.modal_stack == 0 then
      M.push_modal("root", { children = M.tree })
    end
  end)
end

function M.create_child_modal(path, node)
  local modal = hs.hotkey.modal.new()

  modal:bind("", "escape", function()
    M.exit_all()
  end)

  for key, child in pairs(node.children or {}) do
    modal:bind("", key, function()
      M.reset_timeout()
      if child.type == "leader" then
        M.push_modal(path .. "." .. key, child)
      elseif child.type == "bind" then
        local result = M.execute_action(child.action)

        -- If sticky, keep modal open; otherwise exit
        if not child.sticky then
          M.exit_all()
        end

        if type(result) == "string" then
          local handler = M.mode_handlers[result]
          if handler and type(handler) == "function" then
            handler()
          end
        end
      end
    end)
  end

  -- Add 'q' as quit key for sticky modals
  modal:bind("", "q", function()
    M.exit_all()
  end)

  local original_entered = function()
    M.show_overlay(path, node)
    M.reset_timeout()
  end

  local original_exited = function()
    if #M.modal_stack == 0 then
      M.hide_overlay()
      M.cancel_timeout()
    end
  end

  modal.entered = original_entered
  modal.exited = original_exited

  return modal, original_entered, original_exited
end

function M.push_modal(path, node)
  local modal = M.modals[path]
  local original_entered, original_exited

  if not modal then
    modal, original_entered, original_exited = M.create_child_modal(path, node)
    M.modals[path] = { modal = modal, original_entered = original_entered, original_exited = original_exited }
  else
    original_entered = modal.original_entered
    original_exited = modal.original_exited
    modal = modal.modal
  end

  local current = M.modal_stack[#M.modal_stack]
  if current then
    current.modal.exited = function() end
    current.modal:exit()
    current.modal.exited = current.original_exited
  end

  table.insert(M.modal_stack, {
    modal = modal,
    path = path,
    node = node,
    original_entered = original_entered,
    original_exited = original_exited
  })

  modal:enter()
end

function M.exit_all()
  while #M.modal_stack > 0 do
    local entry = table.remove(M.modal_stack)
    entry.modal:exit()
  end
  M.hide_overlay()
  M.cancel_timeout()
end

function M.reset_timeout()
  M.cancel_timeout()
  M.timeout_timer = hs.timer.doAfter(M.config.timeout_ms / 1000, function()
    M.exit_all()
  end)
end

function M.cancel_timeout()
  if M.timeout_timer then
    M.timeout_timer:stop()
    M.timeout_timer = nil
  end
end

function M.execute_action(action)
  if not action then return nil end

  if action.url then
    hs.execute("open '" .. action.url .. "'")
    return true
  elseif action.fn then
    if type(action.fn) ~= "function" then
      return nil
    end
    local ok, result = pcall(action.fn)
    if not ok then
      hs.alert("Function execution failed")
      return nil
    end
    return result
  elseif action.shell then
    local ok, err = hs.execute(action.shell)
    if not ok then
      hs.alert("Shell command failed")
      return nil
    end
    return true
  elseif action.focus then
    local ok = hs.application.launchOrFocus(action.focus)
    if not ok then
      hs.alert("Failed to focus application: " .. action.focus)
      return nil
    end
    return true
  elseif action.mode then
    return action.mode
  end
end

function M.show_overlay(path, node)
  if M.canvas then
    M.canvas:hide()
    M.canvas = nil
  end

  local screen = hs.mouse.getCurrentScreen()
  if not screen then return end

  local frame = screen:frame()

  local bindings = {}
  for key, child in pairs(node.children or {}) do
    local is_group = child.type == "leader"
    local is_sticky = child.type == "bind" and child.sticky or false
    table.insert(bindings, {
      key = key,
      desc = child.desc or key,
      is_group = is_group,
      is_sticky = is_sticky,
    })
  end

  table.sort(bindings, function(a, b)
    if a.is_group ~= b.is_group then
      return a.is_group
    end
    return a.key < b.key
  end)

  -- Check if any bindings are sticky
  local has_sticky = false
  for _, binding in ipairs(bindings) do
    if binding.is_sticky then
      has_sticky = true
      break
    end
  end

  -- Add quit keys
  if has_sticky then
    table.insert(bindings, { key = "q", desc = "Quit", is_escape = true })
    table.insert(bindings, { key = "esc", desc = "Quit", is_escape = true })
  else
    table.insert(bindings, { key = "esc", desc = "Cancel", is_escape = true })
  end

  local columns = M.config.overlay_columns
  local rows = math.ceil(#bindings / columns)
  local col_width = 150
  local row_height = 24
  local padding = 16
  local header_height = 36

  local canvas_width = columns * col_width + padding * 2
  local canvas_height = header_height + rows * row_height + padding * 2

  local x = frame.x + frame.w / 2 - canvas_width / 2
  local y = frame.y + frame.h - canvas_height - 50

  M.canvas = hs.canvas.new({ x = x, y = y, w = canvas_width, h = canvas_height })

  M.canvas[1] = {
    type = "rectangle",
    action = "fill",
    fillColor = { alpha = 0.95, red = 0.1, green = 0.1, blue = 0.12 },
    roundedRectRadii = { xRadius = 12, yRadius = 12 }
  }

  local header_text = path == "root" and "Leader" or path:gsub("%.", " > ")

  -- Add STICKY indicator if any bindings are sticky
  local has_sticky = false
  for _, binding in ipairs(bindings) do
    if binding.is_sticky then
      has_sticky = true
      header_text = header_text .. " [STICKY]"
      break
    end
  end

  M.canvas[2] = {
    type = "text",
    text = header_text,
    textColor = { hex = "#88C0D0" },
    textSize = 16,
    textFont = "Menlo",
    frame = { x = padding, y = padding / 2, w = canvas_width - padding * 2, h = header_height }
  }

  local idx = 3
  for i, binding in ipairs(bindings) do
    local col = (i - 1) % columns
    local row = math.floor((i - 1) / columns)

    local key_color = binding.is_escape and { hex = "#BF616A" }
        or binding.is_group and { hex = "#A3BE8C" }
        or { hex = "#EBCB8B" }

    local suffix = binding.is_group and " +" or ""

    M.canvas[idx] = {
      type = "text",
      text = binding.key,
      textColor = key_color,
      textSize = 14,
      textFont = "Menlo",
      frame = {
        x = padding + col * col_width,
        y = header_height + row * row_height,
        w = 30,
        h = row_height
      }
    }
    idx = idx + 1

    M.canvas[idx] = {
      type = "text",
      text = binding.desc .. suffix,
      textColor = { white = 0.85 },
      textSize = 13,
      textFont = "Menlo",
      frame = {
        x = padding + col * col_width + 35,
        y = header_height + row * row_height,
        w = col_width - 40,
        h = row_height
      }
    }
    idx = idx + 1
  end

  M.canvas:show()
end

function M.hide_overlay()
  if M.canvas then
    M.canvas:hide()
    M.canvas = nil
  end
end

return M
