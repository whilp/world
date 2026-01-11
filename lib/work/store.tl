-- State container for work items
-- Replaces module-level data.items with explicit state management

local record WorkItem
  id: string
  title: string
  created: string
  blocks: {string}
  completed: string
  started: string
  description: string
  due: string
  priority: number
  log: {string:string}
  _meta: WorkItemMeta
end

local record WorkItemMeta
  source: string
  kind: string
end

local record Store
  items: {string:WorkItem}
  loaded: boolean
  data_dir: string
end

local record M
  new: function(): Store
  reset: function(store: Store): Store
  add: function(store: Store, item: WorkItem): Store
  get: function(store: Store, id: string): WorkItem
  get_all: function(store: Store): {WorkItem}
  get_by_source: function(store: Store, source: string): {WorkItem}
  is_loaded: function(store: Store): boolean
  mark_loaded: function(store: Store): Store
end

local M_impl: M = {}

-- Create a new store instance
M_impl.new = function(): Store
  return {
    items = {},
    loaded = false,
    data_dir = nil,
  }
end

-- Reset store to empty state
M_impl.reset = function(store: Store): Store
  store.items = {}
  store.loaded = false
  return store
end

-- Add item to store
M_impl.add = function(store: Store, item: WorkItem): Store
  if not item or not item.id then
    error("cannot add item without id")
  end
  store.items[item.id] = item
  return store
end

-- Get item by ID
M_impl.get = function(store: Store, id: string): WorkItem
  return store.items[id]
end

-- Get all items as sorted array
M_impl.get_all = function(store: Store): {WorkItem}
  local all: {WorkItem} = {}
  for _, item in pairs(store.items) do
    table.insert(all, item)
  end
  table.sort(all, function(a: WorkItem, b: WorkItem): boolean
    return (a.created or "") < (b.created or "")
  end)
  return all
end

-- Get items from specific source file
M_impl.get_by_source = function(store: Store, source: string): {WorkItem}
  local items: {WorkItem} = {}
  for _, item in pairs(store.items) do
    if item._meta and item._meta.source == source then
      table.insert(items, item)
    end
  end
  return items
end

-- Check if store has been loaded
M_impl.is_loaded = function(store: Store): boolean
  return store.loaded
end

-- Mark store as loaded
M_impl.mark_loaded = function(store: Store): Store
  store.loaded = true
  return store
end

return M_impl
