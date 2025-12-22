-- work.nvim - minimal configuration for work system
local M = {}

-- Find work data directory
local function find_work_dir()
  local search_paths = {
    "~/*/progress/work",
    "~/progress/work",
  }
  for _, pattern in ipairs(search_paths) do
    local expanded = vim.fn.expand(pattern)
    local matches = vim.fn.glob(expanded, false, true)
    if #matches > 0 and vim.fn.isdirectory(matches[1]) == 1 then
      return matches[1]
    end
  end
  return vim.fn.expand("~/progress/work")
end

-- Ensure work library is in path
local function ensure_lib_path()
  local lib_path = vim.fn.expand("~/lib")
  if not package.path:find(lib_path, 1, true) then
    package.path = lib_path .. "/?.lua;" .. package.path
  end
end

ensure_lib_path()

M.config = {
  data_dir = find_work_dir(),
}

-- Initialize API once and export
local api_module = require("work.api")
M.api = api_module.init({ data_dir = M.config.data_dir })

return M
