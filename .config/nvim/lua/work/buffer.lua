-- work.nvim - buffer management for work items
local M = {}

-- Ensure work library is in path
local lib_path = vim.fn.expand("~/.local/lib/lua")
if not package.path:find(lib_path, 1, true) then
  package.path = lib_path .. "/?.lua;" .. package.path
end

local api_module = require("work.api")
local api = api_module.init({ data_dir = vim.fn.expand("~/stripe/progress/work") })
local render = require("work.render")
local git = require("work.git")
local util = require("work.util")

-- Helper to get short ID from item
local function short_id(item)
  if item._computed and item._computed.short_id then
    return item._computed.short_id:lower()
  end
  return item.id:sub(-6):lower()
end

-- Open work item file by ID
function M.open(id)
  local path, err = api.get_file_path(id)
  if not path then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return false
  end
  vim.cmd.edit(path)
  return true
end

-- Show item details in a scratch buffer
function M.show(id)
  local item, err = api.get(id)
  if not item then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return false
  end
  local detail = render.detail(item)
  local lines = vim.split(detail, "\n")
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.api.nvim_buf_set_option(buf, "buftype", "nofile")
  vim.api.nvim_buf_set_option(buf, "bufhidden", "wipe")
  vim.api.nvim_buf_set_option(buf, "filetype", "work")
  vim.api.nvim_buf_set_name(buf, "work:" .. short_id(item))
  vim.api.nvim_set_current_buf(buf)
  -- Set keybinding to open the actual file
  vim.keymap.set("n", "<CR>", function()
    M.open(item.id)
  end, { buffer = buf, desc = "Open work item file" })
  -- Set keybinding to mark done
  vim.keymap.set("n", "d", function()
    local ok, done_err = api.done(item.id)
    if ok then
      vim.notify("marked done: " .. short_id(item))
      git.commit(item.id, "mark done")
      M.show(item.id) -- refresh
    else
      vim.notify("work: " .. done_err, vim.log.levels.ERROR)
    end
  end, { buffer = buf, desc = "Mark work item done" })
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
    local path = api.get_file_path(word)
    if path then
      vim.cmd.edit(path)
      return true
    end
  end
  return false
end

return M
