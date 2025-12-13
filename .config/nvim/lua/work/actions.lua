-- work.nvim - user-facing actions
local M = {}

local api = require("work").api
local render = require("work.render")
local buffer = require("work.buffer")
local util = require("work.util")


-- Mark current item as done
function M.done(id)
  id = id or util.get_current_id()
  if not id then
    vim.notify("work: no item ID found", vim.log.levels.WARN)
    return
  end
  local item, err = api.done(id)
  if not item then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end
  vim.notify("marked done: " .. util.short_id(item))
  -- Reload buffer if we're viewing the item
  local bufname = vim.api.nvim_buf_get_name(0)
  if bufname:match(id) or bufname:match(item.id) then
    vim.cmd.edit()
  end
end


-- Set due date for item
function M.set_due(id, due_date)
  id = id or util.get_current_id()
  if not id then
    vim.notify("work: no item ID found", vim.log.levels.WARN)
    return
  end
  if not due_date then
    local item, err = api.get(id)
    if not item then
      vim.notify("work: " .. err, vim.log.levels.ERROR)
      return
    end

    local current_due = item.due or ""
    local buf, win = util.create_float_window({
      "# set due date for: " .. item.title,
      "# enter date below (YYYY-MM-DD, 'today', '0d', '1d', '2w', etc.)",
      "# press <CR> or CMD-Enter to save, q to cancel",
      "",
      current_due,
    }, "set due date")

    vim.api.nvim_win_set_cursor(win, {5, #current_due})
    vim.cmd("startinsert!")

    util.setup_float_keymaps(buf, win, function(lines)
      local input_date = nil
      for _, line in ipairs(lines) do
        if not line:match("^%s*#") and not line:match("^%s*$") then
          input_date = line:gsub("^%s*(.-)%s*$", "%1")
          break
        end
      end

      if not input_date or input_date == "" then
        return
      end

      local parsed_date = util.parse_relative_date(input_date)
      if not parsed_date then
        vim.notify("work: invalid date format: " .. input_date, vim.log.levels.ERROR)
        return
      end

      local updated_item, set_err = api.set_due(id, parsed_date)
      if not updated_item then
        vim.notify("work: " .. set_err, vim.log.levels.ERROR)
        return
      end
      vim.notify("set due date: " .. parsed_date)
      -- Reload buffer if viewing the item
      local bufname = vim.api.nvim_buf_get_name(0)
      if bufname:match(id) or bufname:match(updated_item.id) then
        vim.cmd.edit()
      end
    end)

    return
  end

  local parsed_date = util.parse_relative_date(due_date)
  if not parsed_date then
    vim.notify("work: invalid date format: " .. due_date, vim.log.levels.ERROR)
    return
  end

  local item, err = api.set_due(id, parsed_date)
  if not item then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end
  vim.notify("set due date: " .. parsed_date)
  -- Reload buffer if viewing the item
  local bufname = vim.api.nvim_buf_get_name(0)
  if bufname:match(id) or bufname:match(item.id) then
    vim.cmd.edit()
  end
end

-- Add log entry to item
function M.log(id, message)
  id = id or util.get_current_id()
  if not id then
    vim.notify("work: no item ID found", vim.log.levels.WARN)
    return
  end
  if not message then
    local item, err = api.get(id)
    if not item then
      vim.notify("work: " .. err, vim.log.levels.ERROR)
      return
    end

    local buf, win = util.create_float_window({
      "# log entry for: " .. item.title,
      "# press <CR> or CMD-Enter to save, q to cancel",
      "",
    }, "work log")

    vim.api.nvim_win_set_cursor(win, {3, 0})
    vim.cmd("startinsert")

    util.setup_float_keymaps(buf, win, function(lines)
      local log_lines = {}
      for _, line in ipairs(lines) do
        if not line:match("^%s*#") and not line:match("^%s*$") then
          table.insert(log_lines, line)
        end
      end

      if #log_lines > 0 then
        local log_message = table.concat(log_lines, "\n")
        local timestamp, log_err = api.log(id, log_message)
        if not timestamp then
          vim.notify("work: " .. log_err, vim.log.levels.ERROR)
          return
        end
        vim.notify("logged at " .. timestamp)
        -- Reload buffer if viewing the item
        local bufname = vim.api.nvim_buf_get_name(0)
        if bufname:match(id) then
          vim.cmd.edit()
        end
      end
    end)

    return
  end
  local timestamp, err = api.log(id, message)
  if not timestamp then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end
  vim.notify("logged at " .. timestamp)
  -- Reload buffer if viewing the item
  local bufname = vim.api.nvim_buf_get_name(0)
  if bufname:match(id) then
    vim.cmd.edit()
  end
end


-- Delete work item
function M.delete(id)
  id = id or util.get_current_id()
  if not id then
    vim.notify("work: no item ID found", vim.log.levels.WARN)
    return
  end
  local item, err = api.get(id)
  if not item then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end
  vim.ui.select({ "Yes", "No" }, {
    prompt = "Delete '" .. item.title .. "'?",
  }, function(choice)
    if choice == "Yes" then
      local deleted, del_err = api.delete(id)
      if not deleted then
        vim.notify("work: " .. del_err, vim.log.levels.ERROR)
        return
      end
      vim.notify("deleted: " .. util.short_id(item))
      -- Close buffer if viewing the deleted item
      local bufname = vim.api.nvim_buf_get_name(0)
      if bufname:match(item.id) then
        vim.cmd.bdelete()
      end
    end
  end)
end

-- Show item details
function M.show(id)
  id = id or util.get_current_id()
  if not id then
    vim.notify("work: no item ID found", vim.log.levels.WARN)
    return
  end
  require("work.form").edit(id)
end

-- Open item file
function M.open(id)
  id = id or util.get_current_id()
  if not id then
    vim.notify("work: no item ID found", vim.log.levels.WARN)
    return
  end
  buffer.open(id)
end

-- Quick capture: open scratch buffer to add multiple todos
function M.quick_capture()
  local buf, win = util.create_float_window({
    "# add todos below (one per line)",
    "# lines starting with # are ignored",
    "# press <CR> or CMD-Enter to save, q to cancel",
    "",
  }, "quick capture", { height = 8 })

  vim.api.nvim_win_set_cursor(win, {4, 0})
  vim.cmd("startinsert")

  util.setup_float_keymaps(buf, win, function(lines)
    local captured_timestamp = os.date("%Y-%m-%dT%H:%M:%S")
    local count = 0
    local errors = 0
    for _, line in ipairs(lines) do
      if not line:match("^%s*#") and not line:match("^%s*$") then
        local title = line:match("^%s*[-*]%s*(.+)") or line:match("^%s*(.+)")
        if title and title ~= "" then
          local item, err = api.add(title, { captured = captured_timestamp })
          if item then
            count = count + 1
          else
            errors = errors + 1
            vim.notify("work: failed to add '" .. title .. "': " .. err, vim.log.levels.ERROR)
          end
        end
      end
    end

    if count > 0 then
      vim.notify("added " .. count .. " item" .. (count == 1 and "" or "s"))
    end
    if errors > 0 then
      vim.notify("failed to add " .. errors .. " item" .. (errors == 1 and "" or "s"), vim.log.levels.WARN)
    end
  end)
end

-- Add or update blocks for current item
function M.add_blocks(id)
  id = id or util.get_current_id()
  if not id then
    vim.notify("work: no item ID found", vim.log.levels.WARN)
    return
  end

  local item, err = api.get(id)
  if not item then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end

  local picker = require("work.picker")
  picker.select_blocks(item.id, function(selected_ids)
    -- Get existing blocks
    local existing_blocks = item.blocks or {}

    -- Merge with selected blocks (avoiding duplicates)
    local block_set = {}
    for _, block_id in ipairs(existing_blocks) do
      block_set[block_id] = true
    end
    for _, block_id in ipairs(selected_ids) do
      block_set[block_id] = true
    end

    -- Convert back to array
    local new_blocks = {}
    for block_id, _ in pairs(block_set) do
      table.insert(new_blocks, block_id)
    end

    local updated_item, set_err = api.set_blocks(id, new_blocks)
    if not updated_item then
      vim.notify("work: " .. set_err, vim.log.levels.ERROR)
      return
    end

    local count = #selected_ids
    vim.notify("added " .. count .. " block" .. (count == 1 and "" or "s"))

    -- Reload buffer if viewing the item
    local bufname = vim.api.nvim_buf_get_name(0)
    local item_short_id = util.short_id(updated_item)

    -- Reload buffer if viewing the item
    if bufname:match(id) or bufname:match(updated_item.id) or bufname:match(item_short_id) then
      vim.cmd.edit()
    end
  end)
end

return M
