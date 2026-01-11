-- luacheck ignore: neovim runtime
--check:false
-- work.nvim - buffer management for work items
local M = {}

local api = require("work").api
local util = require("work.util")

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
