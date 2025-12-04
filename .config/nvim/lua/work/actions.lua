-- work.nvim - user-facing actions
local M = {}

local work = require("work")
local buffer = require("work.buffer")

-- Create a floating window with common settings and keymappings
-- Returns: buf, win
local function create_float_window(lines, title)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)

  local width = math.min(80, vim.o.columns - 10)
  local height = math.min(20, vim.o.lines - 10)
  local row = math.floor((vim.o.lines - height) / 2)
  local col = math.floor((vim.o.columns - width) / 2)

  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = height,
    row = row,
    col = col,
    style = "minimal",
    border = "rounded",
    title = " " .. title .. " ",
    title_pos = "center",
  })

  vim.bo[buf].filetype = "markdown"
  vim.api.nvim_win_set_option(win, "cursorline", true)
  vim.api.nvim_buf_set_option(buf, "bufhidden", "wipe")

  return buf, win
end

-- Setup keymappings for floating window
-- process_fn: function to call on save
local function setup_float_keymaps(buf, win, process_fn)
  local function process_and_close()
    local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    vim.api.nvim_win_close(win, true)
    process_fn(lines)
  end

  vim.keymap.set("n", "<CR>", process_and_close, {buffer = buf})
  vim.keymap.set("i", "<D-CR>", process_and_close, {buffer = buf})
  vim.keymap.set("n", "q", function()
    vim.api.nvim_win_close(win, true)
  end, {buffer = buf})
end

-- Get work item ID from current context
-- Returns: id or nil
local function get_current_id()
  local bufname = vim.api.nvim_buf_get_name(0)
  -- Check if we're in a work item file
  if bufname:match("/work/[%a%d]+%.lua$") then
    local id = bufname:match("([%a%d]+)%.lua$")
    return id
  end
  -- Check if we're in a work buffer with ID in name
  if bufname:match("^work:") then
    local id = bufname:match(":([%a%d]+)$")
    if id and #id >= 6 then
      return id
    end
  end
  -- Try word under cursor
  local word = vim.fn.expand("<cword>")
  if word:match("^[%a%d]+$") and #word >= 6 then
    local _, err = work.get(word)
    if not err then
      return word
    end
  end
  return nil
end

-- Mark current item as done
function M.done(id)
  id = id or get_current_id()
  if not id then
    vim.notify("work: no item ID found", vim.log.levels.WARN)
    return
  end
  local item, err = work.mark_done(id)
  if not item then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end
  vim.notify("marked done: " .. work.short_id(item))
  -- Reload buffer if we're viewing the item
  local bufname = vim.api.nvim_buf_get_name(0)
  if bufname:match(id) or bufname:match(item.id) then
    vim.cmd.edit()
  end
end

-- Parse relative date strings like 'today', '0d', '1d', '2w', etc.
-- Returns: YYYY-MM-DD formatted date string or nil
local function parse_relative_date(input)
  if not input or input == "" then
    return nil
  end

  input = input:lower():gsub("^%s*(.-)%s*$", "%1")

  if input == "today" or input == "0d" then
    return os.date("%Y-%m-%d")
  end

  if input == "tomorrow" or input == "1d" then
    return os.date("%Y-%m-%d", os.time() + 86400)
  end

  local num, unit = input:match("^(%d+)([dwmy])$")
  if num and unit then
    num = tonumber(num)
    local seconds_map = {
      d = 86400,
      w = 86400 * 7,
      m = 86400 * 30,
      y = 86400 * 365,
    }
    local seconds = seconds_map[unit]
    if seconds then
      return os.date("%Y-%m-%d", os.time() + (num * seconds))
    end
  end

  if input:match("^%d%d%d%d%-%d%d%-%d%d$") then
    return input
  end

  return nil
end

-- Set due date for item
function M.set_due(id, due_date)
  id = id or get_current_id()
  if not id then
    vim.notify("work: no item ID found", vim.log.levels.WARN)
    return
  end
  if not due_date then
    local item, err = work.get(id)
    if not item then
      vim.notify("work: " .. err, vim.log.levels.ERROR)
      return
    end

    local current_due = item.due or ""
    local buf, win = create_float_window({
      "# set due date for: " .. item.title,
      "# enter date below (YYYY-MM-DD, 'today', '0d', '1d', '2w', etc.)",
      "# press <CR> or CMD-Enter to save, q to cancel",
      "",
      current_due,
    }, "set due date")

    vim.api.nvim_win_set_cursor(win, {5, #current_due})
    vim.cmd("startinsert!")

    setup_float_keymaps(buf, win, function(lines)
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

      local parsed_date = parse_relative_date(input_date)
      if not parsed_date then
        vim.notify("work: invalid date format: " .. input_date, vim.log.levels.ERROR)
        return
      end

      local updated_item, set_err = work.set_due(id, parsed_date)
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

  local parsed_date = parse_relative_date(due_date)
  if not parsed_date then
    vim.notify("work: invalid date format: " .. due_date, vim.log.levels.ERROR)
    return
  end

  local item, err = work.set_due(id, parsed_date)
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
  id = id or get_current_id()
  if not id then
    vim.notify("work: no item ID found", vim.log.levels.WARN)
    return
  end
  if not message then
    local item, err = work.get(id)
    if not item then
      vim.notify("work: " .. err, vim.log.levels.ERROR)
      return
    end

    local buf, win = create_float_window({
      "# log entry for: " .. item.title,
      "# press <CR> or CMD-Enter to save, q to cancel",
      "",
    }, "work log")

    vim.api.nvim_win_set_cursor(win, {3, 0})
    vim.cmd("startinsert")

    setup_float_keymaps(buf, win, function(lines)
      local log_lines = {}
      for _, line in ipairs(lines) do
        if not line:match("^%s*#") and not line:match("^%s*$") then
          table.insert(log_lines, line)
        end
      end

      if #log_lines > 0 then
        local log_message = table.concat(log_lines, "\n")
        local timestamp, log_err = work.add_log(id, log_message)
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
  local timestamp, err = work.add_log(id, message)
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
  id = id or get_current_id()
  if not id then
    vim.notify("work: no item ID found", vim.log.levels.WARN)
    return
  end
  local item, err = work.get(id)
  if not item then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end
  vim.ui.select({ "Yes", "No" }, {
    prompt = "Delete '" .. item.title .. "'?",
  }, function(choice)
    if choice == "Yes" then
      local deleted, del_err = work.delete(id)
      if not deleted then
        vim.notify("work: " .. del_err, vim.log.levels.ERROR)
        return
      end
      vim.notify("deleted: " .. work.short_id(item))
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
  id = id or get_current_id()
  if not id then
    vim.notify("work: no item ID found", vim.log.levels.WARN)
    return
  end
  buffer.show(id)
end

-- Open item file
function M.open(id)
  id = id or get_current_id()
  if not id then
    vim.notify("work: no item ID found", vim.log.levels.WARN)
    return
  end
  buffer.open(id)
end

-- Quick capture: open scratch buffer to add multiple todos
function M.quick_capture()
  local buf, win = create_float_window({
    "# add todos below (one per line)",
    "# lines starting with # are ignored",
    "# press <CR> or CMD-Enter to save, q to cancel",
    "",
  }, "quick capture")

  vim.api.nvim_win_set_cursor(win, {4, 0})
  vim.cmd("startinsert")

  setup_float_keymaps(buf, win, function(lines)
    local captured_timestamp = os.date("%Y-%m-%dT%H:%M:%S")
    local count = 0
    local errors = 0
    for _, line in ipairs(lines) do
      if not line:match("^%s*#") and not line:match("^%s*$") then
        local title = line:match("^%s*[-*]%s*(.+)") or line:match("^%s*(.+)")
        if title and title ~= "" then
          local item, err = work.add(title, { captured = captured_timestamp })
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

return M
