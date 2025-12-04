-- work.nvim - buffer management for work items
local M = {}

local work = require("work")

-- Open work item file by ID
function M.open(id)
  local path, err = work.get_file_path(id)
  if not path then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return false
  end
  vim.cmd.edit(path)
  return true
end

-- Show item details in a scratch buffer
function M.show(id)
  local item, err = work.get(id)
  if not item then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return false
  end
  local detail = work.render_detail(item)
  local lines = vim.split(detail, "\n")
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.api.nvim_buf_set_option(buf, "buftype", "nofile")
  vim.api.nvim_buf_set_option(buf, "bufhidden", "wipe")
  vim.api.nvim_buf_set_option(buf, "filetype", "work")
  vim.api.nvim_buf_set_name(buf, "work:" .. work.short_id(item))
  vim.api.nvim_set_current_buf(buf)
  -- Set keybinding to open the actual file
  vim.keymap.set("n", "<CR>", function()
    M.open(item.id)
  end, { buffer = buf, desc = "Open work item file" })
  -- Set keybinding to mark done
  vim.keymap.set("n", "d", function()
    local ok, done_err = work.mark_done(item.id)
    if ok then
      vim.notify("marked done: " .. work.short_id(item))
      M.show(item.id) -- refresh
    else
      vim.notify("work: " .. done_err, vim.log.levels.ERROR)
    end
  end, { buffer = buf, desc = "Mark work item done" })
  return true
end

-- Show tree view in a scratch buffer
function M.tree(root_id)
  local tree_data, err = work.get_tree(root_id)
  if not tree_data then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return false
  end
  local rendered = work.render_tree(tree_data)
  local lines = vim.split(rendered, "\n")
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.api.nvim_buf_set_option(buf, "buftype", "nofile")
  vim.api.nvim_buf_set_option(buf, "bufhidden", "wipe")
  vim.api.nvim_buf_set_option(buf, "filetype", "work")
  vim.api.nvim_buf_set_name(buf, root_id and ("work:tree:" .. root_id) or "work:tree")
  vim.api.nvim_set_current_buf(buf)
  -- Set keybinding to open item under cursor
  vim.keymap.set("n", "<CR>", function()
    local line = vim.api.nvim_get_current_line()
    local id = line:match("[%a%d][%a%d][%a%d][%a%d][%a%d][%a%d]")
    if id then
      M.open(id)
    end
  end, { buffer = buf, desc = "Open work item under cursor" })
  -- Set keybinding to show item details
  vim.keymap.set("n", "K", function()
    local line = vim.api.nvim_get_current_line()
    local id = line:match("[%a%d][%a%d][%a%d][%a%d][%a%d][%a%d]")
    if id then
      M.show(id)
    end
  end, { buffer = buf, desc = "Show work item details" })
  return true
end

-- Show ready items in a scratch buffer
function M.ready()
  local items, err = work.get_ready()
  if not items then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return false
  end
  local rendered = work.render_ready(items)
  local lines = vim.split(rendered, "\n")
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.api.nvim_buf_set_option(buf, "buftype", "nofile")
  vim.api.nvim_buf_set_option(buf, "bufhidden", "wipe")
  vim.api.nvim_buf_set_option(buf, "filetype", "work")
  vim.api.nvim_buf_set_name(buf, "work:ready")
  vim.api.nvim_set_current_buf(buf)
  -- Set keybindings
  vim.keymap.set("n", "<CR>", function()
    local line = vim.api.nvim_get_current_line()
    local id = line:match("[%a%d][%a%d][%a%d][%a%d][%a%d][%a%d]")
    if id then
      M.open(id)
    end
  end, { buffer = buf, desc = "Open work item under cursor" })
  vim.keymap.set("n", "K", function()
    local line = vim.api.nvim_get_current_line()
    local id = line:match("[%a%d][%a%d][%a%d][%a%d][%a%d][%a%d]")
    if id then
      M.show(id)
    end
  end, { buffer = buf, desc = "Show work item details" })
  return true
end

-- Show blocked items in a scratch buffer
function M.blocked()
  local items, err = work.get_blocked()
  if not items then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return false
  end
  local rendered = work.render_blocked(items)
  local lines = vim.split(rendered, "\n")
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.api.nvim_buf_set_option(buf, "buftype", "nofile")
  vim.api.nvim_buf_set_option(buf, "bufhidden", "wipe")
  vim.api.nvim_buf_set_option(buf, "filetype", "work")
  vim.api.nvim_buf_set_name(buf, "work:blocked")
  vim.api.nvim_set_current_buf(buf)
  -- Set keybindings
  vim.keymap.set("n", "<CR>", function()
    local line = vim.api.nvim_get_current_line()
    local id = line:match("[%a%d][%a%d][%a%d][%a%d][%a%d][%a%d]")
    if id then
      M.open(id)
    end
  end, { buffer = buf, desc = "Open work item under cursor" })
  vim.keymap.set("n", "K", function()
    local line = vim.api.nvim_get_current_line()
    local id = line:match("[%a%d][%a%d][%a%d][%a%d][%a%d][%a%d]")
    if id then
      M.show(id)
    end
  end, { buffer = buf, desc = "Show work item details" })
  return true
end

-- Show all items in a list buffer
function M.list()
  local items, err = work.get_all_enriched()
  if not items then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return false
  end
  local rendered = work.render_list(items)
  local lines = vim.split(rendered, "\n")
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.api.nvim_buf_set_option(buf, "buftype", "nofile")
  vim.api.nvim_buf_set_option(buf, "bufhidden", "wipe")
  vim.api.nvim_buf_set_option(buf, "filetype", "work")
  vim.api.nvim_buf_set_name(buf, "work:list")
  vim.api.nvim_set_current_buf(buf)
  -- Set keybindings
  vim.keymap.set("n", "<CR>", function()
    local line = vim.api.nvim_get_current_line()
    local id = line:match("[%a%d][%a%d][%a%d][%a%d][%a%d][%a%d]")
    if id then
      M.open(id)
    end
  end, { buffer = buf, desc = "Open work item under cursor" })
  vim.keymap.set("n", "K", function()
    local line = vim.api.nvim_get_current_line()
    local id = line:match("[%a%d][%a%d][%a%d][%a%d][%a%d][%a%d]")
    if id then
      M.show(id)
    end
  end, { buffer = buf, desc = "Show work item details" })
  return true
end

-- Try to open work item under cursor (for gf mapping)
function M.goto_item()
  local word = vim.fn.expand("<cword>")
  if not word or word == "" then
    return false
  end
  -- Try to resolve as work ID (6+ chars alphanumeric)
  if word:match("^[%a%d]+$") and #word >= 6 then
    local path = work.get_file_path(word)
    if path then
      vim.cmd.edit(path)
      return true
    end
  end
  return false
end

return M
