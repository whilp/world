-- Unified API layer for work system
-- Provides high-level operations for both CLI and nvim consumers

local config = require("work.config")
local data = require("work.data")
local store = require("work.store")
local process = require("work.process")
local validate = require("work.validate")
local render = require("work.render")

local M = {}

-- Module state (singleton pattern)
local _store = store.new()
local _config = nil

-- Initialize API with configuration
-- opts.data_dir: explicit data directory path
-- opts.search_patterns: custom search patterns for auto-discovery
-- Returns: api module for chaining
M.init = function(opts)
  _config = config.get(opts)
  _store.data_dir = _config.data_dir
  return M
end

-- Load all items from disk
-- Returns: ok, err
M.load = function()
  if store.is_loaded(_store) then
    return true
  end

  if not _config then
    M.init({})
  end

  store.reset(_store)
  local ok, err = data.load_all(_store, _config.data_dir)
  if not ok then
    return nil, err
  end

  store.mark_loaded(_store)
  return true
end

-- Resolve short ID to full ID
-- Returns: full_id, err
M.resolve_id = function(short_id)
  local ok, err = M.load()
  if not ok then
    return nil, err
  end
  return data.resolve_id(_store, short_id)
end

-- Get single item by ID (enriched)
-- Returns: item, err
M.get = function(id)
  local ok, err = M.load()
  if not ok then
    return nil, err
  end

  local full_id, resolve_err = data.resolve_id(_store, id)
  if not full_id then
    return nil, resolve_err
  end

  local item = data.get(_store, full_id)
  if not item then
    return nil, "item not found: " .. full_id
  end

  return process.enrich(_store, item)
end

-- Get all items (enriched, sorted)
-- Returns: items array, err
M.get_all = function()
  local ok, err = M.load()
  if not ok then
    return nil, err
  end

  local items = data.get_all(_store)
  return process.enrich_all(_store, items)
end

-- Get ready (unblocked) items
-- Returns: items array, err
M.get_ready = function()
  local ok, err = M.load()
  if not ok then
    return nil, err
  end

  local items = process.get_ready_items(_store)
  process.sort_by_schedule(items)
  return process.enrich_all(_store, items)
end

-- Get blocked items
-- Returns: items array, err
M.get_blocked = function()
  local ok, err = M.load()
  if not ok then
    return nil, err
  end

  local items = process.get_blocked_items(_store)
  process.sort_by_schedule(items)
  return process.enrich_all(_store, items)
end

-- Get incomplete items
-- Returns: items array, err
M.get_incomplete = function()
  local ok, err = M.load()
  if not ok then
    return nil, err
  end

  local items = process.get_incomplete_items(_store)
  process.sort_by_schedule(items)
  return process.enrich_all(_store, items)
end

-- Get dependency tree
-- root_id: optional root ID to start from
-- Returns: tree_data, err
M.get_tree = function(root_id)
  local ok, err = M.load()
  if not ok then
    return nil, err
  end

  local items
  if root_id then
    local full_id, resolve_err = data.resolve_id(_store, root_id)
    if not full_id then
      return nil, resolve_err
    end

    local root = data.get(_store, full_id)
    if not root then
      return nil, "item not found: " .. full_id
    end

    items = { root }
    local dependents = process.get_transitive_dependents(_store, full_id)
    for _, item in ipairs(dependents) do
      table.insert(items, item)
    end
  else
    items = data.get_all(_store)
  end

  local enriched = process.enrich_all(_store, items)
  local tree = process.build_tree(enriched)

  process.sort_by_schedule(tree.roots)
  for _, children in pairs(tree.children) do
    process.sort_by_schedule(children)
  end

  return tree
end

-- Add new work item
-- title: item title (required)
-- opts: optional fields (description, due, priority, blocks)
-- Returns: item, err
M.add = function(title, opts)
  local ok, err = M.load()
  if not ok then
    return nil, err
  end

  opts = opts or {}

  local item = {
    id = data.generate_id(),
    title = title,
    created = os.date("%Y-%m-%d"),
  }

  if opts.description then
    item.description = opts.description
  end

  if opts.due then
    local ok, err = validate.due_date(opts.due)
    if not ok then
      return nil, err
    end
    item.due = opts.due
  end

  if opts.priority then
    item.priority = opts.priority
  end

  if opts.captured then
    item.captured = opts.captured
  end

  if opts.blocks then
    local blocks = {}
    for _, short_id in ipairs(opts.blocks) do
      local full_id, resolve_err = data.resolve_id(_store, short_id)
      if not full_id then
        return nil, resolve_err
      end
      table.insert(blocks, full_id)
    end

    local ok, validate_err = process.validate_blocks(_store, item.id, blocks)
    if not ok then
      return nil, validate_err
    end

    item.blocks = blocks
  end

  store.add(_store, item)

  local ok, save_err = data.save(item, _config.data_dir)
  if not ok then
    return nil, save_err
  end

  return item
end

-- Mark item as done
-- Returns: item, err
M.done = function(id)
  local ok, err = M.load()
  if not ok then
    return nil, err
  end

  local full_id, resolve_err = data.resolve_id(_store, id)
  if not full_id then
    return nil, resolve_err
  end

  local item = data.get(_store, full_id)
  if not item then
    return nil, "item not found: " .. full_id
  end

  data.mark_done(item)

  local ok, save_err = data.save(item, _config.data_dir)
  if not ok then
    return nil, save_err
  end

  return item
end

-- Update item fields
-- id: item ID
-- updates: table of field=value pairs
-- Returns: item, err
M.update = function(id, updates)
  local ok, err = M.load()
  if not ok then
    return nil, err
  end

  local full_id, resolve_err = data.resolve_id(_store, id)
  if not full_id then
    return nil, resolve_err
  end

  local item = data.get(_store, full_id)
  if not item then
    return nil, "item not found: " .. full_id
  end

  for key, value in pairs(updates) do
    -- Empty value means remove the field
    if value == "" or value == nil then
      item[key] = nil
    else
      local ok, validate_err = validate.field_update(item, key, value)
      if not ok then
        return nil, validate_err
      end

      if key == "blocks" then
        -- Parse blocks if it's a string
        local blocks
        if type(value) == "string" then
          blocks, validate_err = validate.parse_blocks(value, function(short_id)
            return data.resolve_id(_store, short_id)
          end)
          if not blocks then
            return nil, validate_err
          end
        else
          -- Assume it's already an array
          blocks = value
        end

        local ok, cycle_err = process.validate_blocks(_store, item.id, blocks)
        if not ok then
          return nil, cycle_err
        end

        item[key] = blocks
      elseif key == "priority" then
        item[key] = tonumber(value)
      else
        item[key] = value
      end
    end
  end

  local ok, save_err = data.save(item, _config.data_dir)
  if not ok then
    return nil, save_err
  end

  return item
end

-- Add log entry to item
-- Returns: timestamp, err
M.log = function(id, message)
  local ok, err = M.load()
  if not ok then
    return nil, err
  end

  local full_id, resolve_err = data.resolve_id(_store, id)
  if not full_id then
    return nil, resolve_err
  end

  local item = data.get(_store, full_id)
  if not item then
    return nil, "item not found: " .. full_id
  end

  local timestamp = os.date("%Y-%m-%dT%H:%M:%S")
  data.add_log(item, message, timestamp)

  local ok, save_err = data.save(item, _config.data_dir)
  if not ok then
    return nil, save_err
  end

  return timestamp
end

-- Set due date for item
-- Returns: item, err
M.set_due = function(id, due_date)
  return M.update(id, { due = due_date })
end

-- Set blocks for item
-- blocks: array of item IDs or comma-separated string
-- Returns: item, err
M.set_blocks = function(id, blocks)
  return M.update(id, { blocks = blocks })
end

-- Delete item
-- Returns: item (deleted), err
M.delete = function(id)
  local ok, err = M.load()
  if not ok then
    return nil, err
  end

  local full_id, resolve_err = data.resolve_id(_store, id)
  if not full_id then
    return nil, resolve_err
  end

  local item = data.get(_store, full_id)
  if not item then
    return nil, "item not found: " .. full_id
  end

  -- Check for orphaned block references
  local referencing_items = process.find_items_blocking_on(_store, full_id)
  if #referencing_items > 0 then
    io.stderr:write("warning: deleting this item will orphan block references in:\n")
    for _, ref_item in ipairs(referencing_items) do
      local short_id = ref_item.id:sub(-6):lower()
      io.stderr:write(string.format("  - %s: %s\n", short_id, ref_item.title))
    end
    io.stderr:flush()
  end

  local clean_copy = data.clean(item)

  local ok, delete_err = data.delete(item, _config.data_dir)
  if not ok then
    return nil, delete_err
  end

  -- Remove from store
  _store.items[full_id] = nil

  return clean_copy
end

-- Get file path for item
-- Returns: path, err
M.get_file_path = function(id)
  local ok, err = M.load()
  if not ok then
    return nil, err
  end

  local full_id, resolve_err = data.resolve_id(_store, id)
  if not full_id then
    return nil, resolve_err
  end

  local item = data.get(_store, full_id)
  if not item then
    return nil, "item not found: " .. full_id
  end

  return item._meta and item._meta.source or (_config.data_dir .. "/" .. full_id .. ".lua")
end

-- Clean item (remove internal fields)
-- Returns: cleaned item
M.clean = function(item)
  return data.clean(item)
end

-- Expose render functions
M.render = render

return M
