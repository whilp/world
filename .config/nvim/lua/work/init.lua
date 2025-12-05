-- work.nvim - neovim integration for work todo system
-- Main module providing API for work items

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

local function find_lib_path()
  local work_dir = find_work_dir()
  local lib_path = vim.fn.fnamemodify(work_dir, ":h") .. "/lib"
  if vim.fn.isdirectory(lib_path) == 1 then
    return lib_path
  end
  return vim.fn.expand("~/progress/lib")
end

M.config = {
  data_dir = find_work_dir(),
  lib_path = find_lib_path(),
}

-- Ensure work library is in path
local function ensure_lib_path()
  local lib_path = M.config.lib_path
  if not package.path:find(lib_path, 1, true) then
    package.path = lib_path .. "/?.lua;" .. package.path
  end
end

-- Load work library modules with error handling
local function get_data()
  ensure_lib_path()
  local ok, mod = pcall(require, "work.data")
  if not ok then
    return nil, "failed to load work.data: " .. tostring(mod)
  end
  return mod
end

local function get_process()
  ensure_lib_path()
  local ok, mod = pcall(require, "work.process")
  if not ok then
    return nil, "failed to load work.process: " .. tostring(mod)
  end
  return mod
end

local function get_render()
  ensure_lib_path()
  local ok, mod = pcall(require, "work.render")
  if not ok then
    return nil, "failed to load work.render: " .. tostring(mod)
  end
  return mod
end

-- Load all work items from data directory
-- Returns: items table (id -> item), or nil, error
function M.load_items()
  local data, err = get_data()
  if not data then
    return nil, err
  end
  data.items = {} -- reset
  local ok, load_err = data.load_all(M.config.data_dir)
  if not ok then
    return nil, load_err
  end
  return data.items
end

-- Get all items as array, sorted by created date
function M.get_all()
  local items, err = M.load_items()
  if not items then
    return nil, err
  end
  local data, data_err = get_data()
  if not data then
    return nil, data_err
  end
  return data.get_all()
end

-- Get all items enriched with computed fields
function M.get_all_enriched()
  local items, err = M.get_all()
  if not items then
    return nil, err
  end
  local process, proc_err = get_process()
  if not process then
    return nil, proc_err
  end
  return process.enrich_all(items)
end

-- Get ready (unblocked) items
function M.get_ready()
  local _, err = M.load_items()
  if err then
    return nil, err
  end
  local process, proc_err = get_process()
  if not process then
    return nil, proc_err
  end
  local items = process.get_ready_items()
  process.sort_by_schedule(items)
  return process.enrich_all(items)
end

-- Get blocked items
function M.get_blocked()
  local _, err = M.load_items()
  if err then
    return nil, err
  end
  local process, proc_err = get_process()
  if not process then
    return nil, proc_err
  end
  local items = process.get_blocked_items()
  process.sort_by_schedule(items)
  return process.enrich_all(items)
end

-- Get item by ID (full or short)
function M.get(id)
  local _, err = M.load_items()
  if err then
    return nil, err
  end
  local data, data_err = get_data()
  if not data then
    return nil, data_err
  end
  local full_id, resolve_err = data.resolve_id(id)
  if not full_id then
    return nil, resolve_err
  end
  local item = data.get(full_id)
  if not item then
    return nil, "item not found: " .. full_id
  end
  local process, proc_err = get_process()
  if not process then
    return nil, proc_err
  end
  return process.enrich(item)
end

-- Get file path for an item ID
function M.get_file_path(id)
  local _, err = M.load_items()
  if err then
    return nil, err
  end
  local data, data_err = get_data()
  if not data then
    return nil, data_err
  end
  local full_id, resolve_err = data.resolve_id(id)
  if not full_id then
    return nil, resolve_err
  end
  local item = data.get(full_id)
  if not item then
    return nil, "item not found: " .. full_id
  end
  return item._meta and item._meta.source or (M.config.data_dir .. "/" .. full_id .. ".lua")
end

-- Mark item as done
function M.mark_done(id)
  local _, err = M.load_items()
  if err then
    return nil, err
  end
  local data, data_err = get_data()
  if not data then
    return nil, data_err
  end
  local full_id, resolve_err = data.resolve_id(id)
  if not full_id then
    return nil, resolve_err
  end
  local item = data.get(full_id)
  if not item then
    return nil, "item not found: " .. full_id
  end
  item.completed = os.date("%Y-%m-%dT%H:%M:%S")
  local ok, save_err = data.save(item, M.config.data_dir)
  if not ok then
    return nil, save_err
  end
  return item
end

-- Add log entry to item
function M.add_log(id, message)
  local _, err = M.load_items()
  if err then
    return nil, err
  end
  local data, data_err = get_data()
  if not data then
    return nil, data_err
  end
  local full_id, resolve_err = data.resolve_id(id)
  if not full_id then
    return nil, resolve_err
  end
  local item = data.get(full_id)
  if not item then
    return nil, "item not found: " .. full_id
  end
  if not item.log then
    item.log = {}
  end
  local timestamp = os.date("%Y-%m-%dT%H:%M:%S")
  item.log[timestamp] = message
  local ok, save_err = data.save(item, M.config.data_dir)
  if not ok then
    return nil, save_err
  end
  return timestamp
end

-- Set due date for item
function M.set_due(id, due_date)
  local _, err = M.load_items()
  if err then
    return nil, err
  end
  local data, data_err = get_data()
  if not data then
    return nil, data_err
  end
  local full_id, resolve_err = data.resolve_id(id)
  if not full_id then
    return nil, resolve_err
  end
  local item = data.get(full_id)
  if not item then
    return nil, "item not found: " .. full_id
  end
  item.due = due_date
  local ok, save_err = data.save(item, M.config.data_dir)
  if not ok then
    return nil, save_err
  end
  return item
end

-- Add or update blocks for an item
function M.set_blocks(id, block_ids)
  local _, err = M.load_items()
  if err then
    return nil, err
  end
  local data, data_err = get_data()
  if not data then
    return nil, data_err
  end
  local full_id, resolve_err = data.resolve_id(id)
  if not full_id then
    return nil, resolve_err
  end
  local item = data.get(full_id)
  if not item then
    return nil, "item not found: " .. full_id
  end
  local process, proc_err = get_process()
  if not process then
    return nil, proc_err
  end

  -- Resolve short IDs to full IDs
  local blocks = {}
  for _, block_id in ipairs(block_ids) do
    local full_block_id, block_resolve_err = data.resolve_id(block_id)
    if not full_block_id then
      return nil, block_resolve_err
    end
    table.insert(blocks, full_block_id)
  end

  -- Validate blocks
  local ok, validate_err = process.validate_blocks(item.id, blocks)
  if not ok then
    return nil, validate_err
  end

  item.blocks = blocks
  local save_ok, save_err = data.save(item, M.config.data_dir)
  if not save_ok then
    return nil, save_err
  end
  return item
end

-- Create new item
function M.add(title, opts)
  opts = opts or {}
  local data, data_err = get_data()
  if not data then
    return nil, data_err
  end
  local _, err = M.load_items()
  if err then
    return nil, err
  end
  local item = {
    id = data.generate_id(),
    title = title,
    created = os.date("%Y-%m-%d"),
  }
  if opts.description then
    item.description = opts.description
  end
  if opts.due then
    item.due = opts.due
  end
  if opts.priority then
    item.priority = opts.priority
  end
  if opts.captured then
    item.captured = opts.captured
  end
  if opts.blocks then
    local process, proc_err = get_process()
  if not process then
    return nil, proc_err
  end
    local blocks = {}
    for _, short_id in ipairs(opts.blocks) do
      local full_id, resolve_err = data.resolve_id(short_id)
      if not full_id then
        return nil, resolve_err
      end
      table.insert(blocks, full_id)
    end
    local ok, validate_err = process.validate_blocks(item.id, blocks)
    if not ok then
      return nil, validate_err
    end
    item.blocks = blocks
  end
  data.items[item.id] = item
  local ok, save_err = data.save(item, M.config.data_dir)
  if not ok then
    return nil, save_err
  end
  return item
end

-- Delete item
function M.delete(id)
  local _, err = M.load_items()
  if err then
    return nil, err
  end
  local data, data_err = get_data()
  if not data then
    return nil, data_err
  end
  local full_id, resolve_err = data.resolve_id(id)
  if not full_id then
    return nil, resolve_err
  end
  local item = data.get(full_id)
  if not item then
    return nil, "item not found: " .. full_id
  end
  local ok, delete_err = data.delete(item, M.config.data_dir)
  if not ok then
    return nil, delete_err
  end
  return item
end

-- Build dependency tree
function M.get_tree(root_id)
  local _, err = M.load_items()
  if err then
    return nil, err
  end
  local data, data_err = get_data()
  if not data then
    return nil, data_err
  end
  local process, proc_err = get_process()
  if not process then
    return nil, proc_err
  end
  local items
  if root_id then
    local full_id, resolve_err = data.resolve_id(root_id)
    if not full_id then
      return nil, resolve_err
    end
    local root = data.get(full_id)
    if not root then
      return nil, "item not found: " .. full_id
    end
    items = { root }
    local dependents = process.get_transitive_dependents(full_id)
    for _, item in ipairs(dependents) do
      table.insert(items, item)
    end
  else
    items = data.get_all()
  end
  local enriched = process.enrich_all(items)
  local tree = process.build_tree(enriched)
  process.sort_by_schedule(tree.roots)
  for _, children in pairs(tree.children) do
    process.sort_by_schedule(children)
  end
  return tree
end

-- Render functions (delegate to render module)
function M.render_list(items)
  local render, render_err = get_render()
  if not render then
    return nil, render_err
  end
  return render.list(items)
end

function M.render_tree(tree_data)
  local render, render_err = get_render()
  if not render then
    return nil, render_err
  end
  return render.tree(tree_data)
end

function M.render_detail(item)
  local render, render_err = get_render()
  if not render then
    return nil, render_err
  end
  return render.detail(item)
end

function M.render_ready(items)
  local render, render_err = get_render()
  if not render then
    return nil, render_err
  end
  return render.ready(items)
end

function M.render_blocked(items)
  local render, render_err = get_render()
  if not render then
    return nil, render_err
  end
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
