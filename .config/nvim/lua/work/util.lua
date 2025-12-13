local M = {}

-- ID regex pattern constant
M.ID_PATTERN = "[%a%d][%a%d][%a%d][%a%d][%a%d][%a%d]"

-- Get window configuration for centered pickers
function M.get_window_config()
  return {
    config = function()
      local height = math.floor(vim.o.lines * 0.5)
      local width = math.floor(vim.o.columns * 0.6)
      return {
        anchor = "NW",
        height = height,
        width = width,
        row = math.floor((vim.o.lines - height) / 2),
        col = math.floor((vim.o.columns - width) / 2),
      }
    end
  }
end

-- Create a floating window with common settings
-- Returns: buf, win
function M.create_float_window(lines, title, opts)
  opts = opts or {}
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)

  local width = math.min(80, vim.o.columns - 10)
  local default_height = opts.height or 20
  local height = math.min(default_height, vim.o.lines - 10)
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
function M.setup_float_keymaps(buf, win, process_fn)
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
function M.get_current_id()
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
    -- Try to resolve it (requires work module)
    local ok, work = pcall(require, "work.api")
    if ok then
      local _, err = work.get(word)
      if not err then
        return word
      end
    end
  end
  return nil
end

-- Parse relative date strings like 'today', '0d', '1d', '2w', '+1d', '-1d', etc
-- Returns YYYY-MM-DD formatted date string or nil
function M.parse_relative_date(input)
  if not input or input == "" then
    return nil
  end

  input = input:lower():gsub("^%s*(.-)%s*$", "%1")

  if input == "today" or input == "0d" then
    return os.date("%Y-%m-%d")
  end

  if input == "tomorrow" or input == "+1d" then
    return os.date("%Y-%m-%d", os.time() + 86400)
  end

  -- Support +/- prefix and d/w/m/y units
  local sign, num, unit = input:match("^([+-]?)(%d+)([dwmy])$")
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
      if sign == "-" then
        return os.date("%Y-%m-%d", os.time() - (num * seconds))
      else
        return os.date("%Y-%m-%d", os.time() + (num * seconds))
      end
    end
  end

  -- Already in YYYY-MM-DD format
  if input:match("^%d%d%d%d%-%d%d%-%d%d$") then
    return input
  end

  return nil
end

-- Format relative days as "+Xd" or "+Xw"
function M.format_relative_days(days)
  if days == 0 then
    return "0d"
  end
  local sign = days > 0 and "+" or ""
  local abs_days = math.abs(days)
  if abs_days % 7 == 0 then
    return sign .. (days / 7) .. "w"
  end
  return sign .. days .. "d"
end

-- Format due date with relative indicator
function M.format_due_date(due_date)
  if not due_date or due_date == "" then
    return "Set due date"
  end

  local year, month, day = due_date:match("(%d+)-(%d+)-(%d+)")
  if not year then
    return "Set due date"
  end

  local due_time = os.time({ year = tonumber(year), month = tonumber(month), day = tonumber(day), hour = 0 })
  local today_time = os.time({ year = os.date("%Y"), month = os.date("%m"), day = os.date("%d"), hour = 0 })
  local days_diff = math.floor((due_time - today_time) / 86400)

  local relative
  if days_diff == 0 then
    relative = "today"
  elseif days_diff == 1 then
    relative = "tomorrow"
  elseif days_diff == -1 then
    relative = "yesterday"
  elseif days_diff < 0 then
    relative = math.abs(days_diff) .. "d ago"
  else
    relative = days_diff .. "d"
  end

  return string.format("%s (%s)", due_date, relative)
end

-- Format timestamp with relative days
function M.format_timestamp(ts, label)
  if not ts or ts == "" then
    return label
  end

  local year, month, day = ts:match("(%d%d%d%d)-(%d%d)-(%d%d)")
  if not year then
    return label
  end

  local ts_time = os.time({ year = tonumber(year), month = tonumber(month), day = tonumber(day), hour = 0 })
  local today_time = os.time({ year = os.date("%Y"), month = os.date("%m"), day = os.date("%d"), hour = 0 })
  local days_diff = math.floor((ts_time - today_time) / 86400)

  return M.format_relative_days(days_diff)
end

return M
