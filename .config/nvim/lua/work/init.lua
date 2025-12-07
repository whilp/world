-- work.nvim - neovim integration for work todo system
-- Thin wrapper around work.api module

local M = {}

-- Configuration
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

M.config = {
  data_dir = find_work_dir(),
}

-- Ensure work library is in path
local function ensure_lib_path()
  local lib_path = vim.fn.expand("~/.local/lib/lua")
  if not package.path:find(lib_path, 1, true) then
    package.path = lib_path .. "/?.lua;" .. package.path
  end
end

ensure_lib_path()

-- Lazy-load API
local _api = nil
local function get_api()
  if not _api then
    local api = require("work.api")
    _api = api.init({ data_dir = M.config.data_dir })
  end
  return _api
end

-- Load all work items from data directory
function M.load_items()
  local api = get_api()
  return api.load()
end

-- Get all items as array, sorted by created date
function M.get_all()
  local api = get_api()
  return api.get_all()
end

-- Get all items enriched with computed fields
function M.get_all_enriched()
  return M.get_all()
end

-- Get ready (unblocked) items
function M.get_ready()
  local api = get_api()
  return api.get_ready()
end

-- Get blocked items
function M.get_blocked()
  local api = get_api()
  return api.get_blocked()
end

-- Get item by ID (full or short)
function M.get(id)
  local api = get_api()
  return api.get(id)
end

-- Get file path for an item ID
function M.get_file_path(id)
  local api = get_api()
  return api.get_file_path(id)
end

-- Mark item as done
function M.mark_done(id)
  local api = get_api()
  return api.done(id)
end

-- Mark item as started
function M.mark_started(id)
  local api = get_api()
  return api.start(id)
end

-- Add log entry to item
function M.add_log(id, message)
  local api = get_api()
  return api.log(id, message)
end

-- Set due date for item
function M.set_due(id, due_date)
  local api = get_api()
  return api.set_due(id, due_date)
end

-- Add or update blocks for an item
function M.set_blocks(id, block_ids)
  local api = get_api()
  return api.set_blocks(id, block_ids)
end

-- Create new item
function M.add(title, opts)
  local api = get_api()
  return api.add(title, opts)
end

-- Delete item
function M.delete(id)
  local api = get_api()
  return api.delete(id)
end

-- Update item fields
function M.update(id, updates)
  local api = get_api()
  return api.update(id, updates)
end

-- Build dependency tree
function M.get_tree(root_id)
  local api = get_api()
  return api.get_tree(root_id)
end

-- Render functions (pass-through to render module)
function M.render_list(items)
  local render = require("work.render")
  return render.list(items)
end

function M.render_tree(tree_data)
  local render = require("work.render")
  return render.tree(tree_data)
end

function M.render_detail(item)
  local render = require("work.render")
  return render.detail(item)
end

function M.render_ready(items)
  local render = require("work.render")
  return render.ready(items)
end

function M.render_blocked(items)
  local render = require("work.render")
  return render.blocked(items)
end

-- Get short ID from item
function M.short_id(item)
  if item._computed and item._computed.short_id then
    return item._computed.short_id:lower()
  end
  return item.id:sub(-6):lower()
end

return M
