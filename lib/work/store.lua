-- State container for work items
-- Replaces module-level data.items with explicit state management

local M = {}

-- Create a new store instance
-- Returns: store
M.new = function()
  return {
    items = {},      -- map of id -> item
    loaded = false,  -- whether items have been loaded from disk
    data_dir = nil,  -- path to data directory
  }
end

-- Reset store to empty state
-- Returns: store
M.reset = function(store)
  store.items = {}
  store.loaded = false
  return store
end

-- Add item to store
-- Returns: store
M.add = function(store, item)
  if not item or not item.id then
    error("cannot add item without id")
  end
  store.items[item.id] = item
  return store
end

-- Get item by ID
-- Returns: item or nil
M.get = function(store, id)
  return store.items[id]
end

-- Get all items as sorted array
-- Returns: array of items sorted by created date
M.get_all = function(store)
  local all = {}
  for _, item in pairs(store.items) do
    table.insert(all, item)
  end
  table.sort(all, function(a, b)
    return (a.created or "") < (b.created or "")
  end)
  return all
end

-- Get items from specific source file
-- Returns: array of items
M.get_by_source = function(store, source)
  local items = {}
  for _, item in pairs(store.items) do
    if item._meta and item._meta.source == source then
      table.insert(items, item)
    end
  end
  return items
end

-- Check if store has been loaded
-- Returns: boolean
M.is_loaded = function(store)
  return store.loaded
end

-- Mark store as loaded
-- Returns: store
M.mark_loaded = function(store)
  store.loaded = true
  return store
end

return M
