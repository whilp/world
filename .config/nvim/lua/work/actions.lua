-- work.nvim - user-facing actions
local M = {}

local work = require("work")
local buffer = require("work.buffer")

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

-- Add log entry to item
function M.log(id, message)
  id = id or get_current_id()
  if not id then
    vim.notify("work: no item ID found", vim.log.levels.WARN)
    return
  end
  if not message then
    vim.ui.input({ prompt = "Log message: " }, function(input)
      if input and input ~= "" then
        M.log(id, input)
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

-- Create new work item
function M.add(title)
  if not title then
    vim.ui.input({ prompt = "Title: " }, function(input)
      if input and input ~= "" then
        M.add(input)
      end
    end)
    return
  end
  local item, err = work.add(title)
  if not item then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end
  vim.notify("created: " .. work.short_id(item))
  -- Open the new item
  local path = work.get_file_path(item.id)
  if path then
    vim.cmd.edit(path)
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
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, {
    "# add todos below (one per line)",
    "# lines starting with # are ignored",
    "# press <CR> to save, q to cancel",
    "",
  })

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
    title = " quick capture ",
    title_pos = "center",
  })

  vim.bo[buf].filetype = "markdown"
  vim.api.nvim_win_set_option(win, "cursorline", true)
  vim.api.nvim_buf_set_option(buf, "bufhidden", "wipe")
  vim.api.nvim_win_set_cursor(win, {4, 0})
  vim.cmd("startinsert")

  local function process_and_close()
    local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    vim.api.nvim_win_close(win, true)

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
  end

  vim.keymap.set("n", "<CR>", process_and_close, {buffer = buf})
  vim.keymap.set("n", "q", function()
    vim.api.nvim_win_close(win, true)
  end, {buffer = buf})
end

return M
