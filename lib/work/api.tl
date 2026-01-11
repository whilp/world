-- Unified API layer for work system
-- Provides high-level operations for both CLI and nvim consumers

local path = require("cosmo.path")

local config = require("work.config")
local data = require("work.data")
local store = require("work.store")
local process = require("work.process")
local validate = require("work.validate")
local render = require("work.render")

-- Types from work.data
local record WorkItemMeta
  source: string
  kind: string
end

local record WorkItemComputed
  short_id: string
  resolved_due: string
  relative_due: string
  is_blocked: boolean
  unresolved_blocks: {string}
  dependent_count: number
end

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
  captured: string
  _meta: WorkItemMeta
  _computed: WorkItemComputed
end

local record WorkStore
  items: {string:WorkItem}
  loaded: boolean
  data_dir: string
end

local record Config
  data_dir: string
end

local record InitOpts
  data_dir: string
  search_patterns: {string}
end

local record AddOpts
  description: string
  due: string
  priority: number
  blocks: {string}
  captured: string
end

local record TreeData
  roots: {WorkItem}
  children: {string:{WorkItem}}
end

-- Module state (singleton pattern)
local _store: WorkStore = store.new() as WorkStore
local _config: Config = nil

local record M
  init: function(opts?: InitOpts): M
  load: function(): boolean, string
  resolve_id: function(short_id: string): string, string
  get: function(id: string): WorkItem, string
  get_all: function(): {WorkItem}, string
  get_ready: function(): {WorkItem}, string
  get_blocked: function(): {WorkItem}, string
  get_incomplete: function(): {WorkItem}, string
  get_tree: function(root_id?: string): TreeData, string
  add: function(title: string, opts?: AddOpts): WorkItem, string
  done: function(id: string): WorkItem, string
  start: function(id: string): WorkItem, string
  update: function(id: string, updates: {string:any}): WorkItem, string
  log: function(id: string, message: string): string, string
  set_due: function(id: string, due_date: string): WorkItem, string
  set_blocks: function(id: string, blocks: {string} | string): WorkItem, string
  delete: function(id: string): WorkItem, string
  get_file_path: function(id: string): string, string
  clean: function(item: WorkItem): WorkItem
  render: any
end

local api: M = {} as M

-- Initialize API with configuration
-- opts.data_dir: explicit data directory path
-- opts.search_patterns: custom search patterns for auto-discovery
-- Returns: api module for chaining
api.init = function(opts?: InitOpts): M
  _config = config.get(opts) as Config
  _store.data_dir = _config.data_dir
  return api
end

-- Load all items from disk
-- Returns: ok, err
api.load = function(): boolean, string
  if store.is_loaded(_store) then
    return true
  end

  if not _config then
    api.init({} as InitOpts)
  end

  store.reset(_store)
  local ok, err = data.load_all(_store as data.WorkStore, _config.data_dir)
  if not ok then
    return nil, err
  end

  store.mark_loaded(_store)
  return true
end

-- Resolve short ID to full ID
-- Returns: full_id, err
api.resolve_id = function(short_id: string): string, string
  local ok, err = api.load()
  if not ok then
    return nil, err
  end
  return data.resolve_id(_store as data.WorkStore, short_id)
end

-- Get single item by ID (enriched)
-- Returns: item, err
api.get = function(id: string): WorkItem, string
  local ok, err = api.load()
  if not ok then
    return nil, err
  end

  local full_id, resolve_err = data.resolve_id(_store as data.WorkStore, id)
  if not full_id then
    return nil, resolve_err
  end

  local item = data.get(_store as data.WorkStore, full_id) as WorkItem
  if not item then
    return nil, "item not found: " .. full_id
  end

  return process.enrich(_store, item) as WorkItem
end

-- Get all items (enriched, sorted)
-- Returns: items array, err
api.get_all = function(): {WorkItem}, string
  local ok, err = api.load()
  if not ok then
    return nil, err
  end

  local items = data.get_all(_store as data.WorkStore) as {WorkItem}
  return process.enrich_all(_store, items) as {WorkItem}
end

-- Get ready (unblocked) items
-- Returns: items array, err
api.get_ready = function(): {WorkItem}, string
  local ok, err = api.load()
  if not ok then
    return nil, err
  end

  local items = process.get_ready_items(_store) as {WorkItem}
  local enriched = process.enrich_all(_store, items) as {WorkItem}
  process.sort_by_schedule(enriched)
  return enriched
end

-- Get blocked items
-- Returns: items array, err
api.get_blocked = function(): {WorkItem}, string
  local ok, err = api.load()
  if not ok then
    return nil, err
  end

  local items = process.get_blocked_items(_store) as {WorkItem}
  local enriched = process.enrich_all(_store, items) as {WorkItem}
  process.sort_by_schedule(enriched)
  return enriched
end

-- Get incomplete items
-- Returns: items array, err
api.get_incomplete = function(): {WorkItem}, string
  local ok, err = api.load()
  if not ok then
    return nil, err
  end

  local items = process.get_incomplete_items(_store) as {WorkItem}
  local enriched = process.enrich_all(_store, items) as {WorkItem}
  process.sort_by_schedule(enriched)
  return enriched
end

-- Get dependency tree
-- root_id: optional root ID to start from
-- Returns: tree_data, err
api.get_tree = function(root_id?: string): TreeData, string
  local ok, err = api.load()
  if not ok then
    return nil, err
  end

  local items: {WorkItem}
  if root_id then
    local full_id, resolve_err = data.resolve_id(_store as data.WorkStore, root_id)
    if not full_id then
      return nil, resolve_err
    end

    local root = data.get(_store as data.WorkStore, full_id) as WorkItem
    if not root then
      return nil, "item not found: " .. full_id
    end

    items = { root }
    local dependents = process.get_transitive_dependents(_store, full_id) as {WorkItem}
    for _, item in ipairs(dependents) do
      table.insert(items, item)
    end
  else
    items = data.get_all(_store as data.WorkStore) as {WorkItem}
  end

  local enriched = process.enrich_all(_store, items) as {WorkItem}
  local tree = process.build_tree(enriched) as TreeData

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
api.add = function(title: string, opts?: AddOpts): WorkItem, string
  local ok, err = api.load()
  if not ok then
    return nil, err
  end

  opts = opts or {} as AddOpts

  local item: WorkItem = {
    id = data.generate_id(),
    title = title,
    created = os.date("%Y-%m-%d") as string,
  }

  if opts.description then
    item.description = opts.description
  end

  if opts.due then
    ok, err = validate.due_date(opts.due)
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
    local blocks: {string} = {}
    for _, short_id in ipairs(opts.blocks) do
      local full_id, resolve_err = data.resolve_id(_store as data.WorkStore, short_id)
      if not full_id then
        return nil, resolve_err
      end
      table.insert(blocks, full_id)
    end

    ok, err = process.validate_blocks(_store, item.id, blocks)
    if not ok then
      return nil, err
    end

    item.blocks = blocks
  end

  store.add(_store, item)

  ok, err = data.save(item as data.WorkItem, _config.data_dir)
  if not ok then
    return nil, err
  end

  return item
end

-- Mark item as done
-- Returns: item, err
api.done = function(id: string): WorkItem, string
  local ok, err = api.load()
  if not ok then
    return nil, err
  end

  local full_id, resolve_err = data.resolve_id(_store as data.WorkStore, id)
  if not full_id then
    return nil, resolve_err
  end

  local item = data.get(_store as data.WorkStore, full_id) as WorkItem
  if not item then
    return nil, "item not found: " .. full_id
  end

  data.mark_done(item as data.WorkItem)

  ok, err = data.save(item as data.WorkItem, _config.data_dir)
  if not ok then
    return nil, err
  end

  return item
end

-- Mark item as started
-- Returns: item, err
api.start = function(id: string): WorkItem, string
  local ok, err = api.load()
  if not ok then
    return nil, err
  end

  local full_id, resolve_err = data.resolve_id(_store as data.WorkStore, id)
  if not full_id then
    return nil, resolve_err
  end

  local item = data.get(_store as data.WorkStore, full_id) as WorkItem
  if not item then
    return nil, "item not found: " .. full_id
  end

  data.mark_started(item as data.WorkItem)

  ok, err = data.save(item as data.WorkItem, _config.data_dir)
  if not ok then
    return nil, err
  end

  return item
end

-- Update item fields
-- id: item ID
-- updates: table of field=value pairs
-- Returns: item, err
api.update = function(id: string, updates: {string:any}): WorkItem, string
  local ok, err = api.load()
  if not ok then
    return nil, err
  end

  local full_id, resolve_err = data.resolve_id(_store as data.WorkStore, id)
  if not full_id then
    return nil, resolve_err
  end

  local item = data.get(_store as data.WorkStore, full_id) as WorkItem
  if not item then
    return nil, "item not found: " .. full_id
  end

  for key, value in pairs(updates) do
    -- Empty value means remove the field
    if value == "" or value == nil then
      (item as {string:any})[key] = nil
    else
      ok, err = validate.field_update(item, key, value)
      if not ok then
        return nil, err
      end

      if key == "blocks" then
        -- Parse blocks if it's a string
        local blocks: {string}
        if type(value) == "string" then
          blocks, err = validate.parse_blocks(value as string, function(short_id: string): string, string
            return data.resolve_id(_store as data.WorkStore, short_id)
          end)
          if not blocks then
            return nil, err
          end
        else
          -- Assume it's already an array
          blocks = value as {string}
        end

        ok, err = process.validate_blocks(_store, item.id, blocks)
        if not ok then
          return nil, err
        end

        item.blocks = blocks
      elseif key == "priority" then
        item.priority = tonumber(value as string)
      else
        (item as {string:any})[key] = value
      end
    end
  end

  ok, err = data.save(item as data.WorkItem, _config.data_dir)
  if not ok then
    return nil, err
  end

  return item
end

-- Add log entry to item
-- Returns: timestamp, err
api.log = function(id: string, message: string): string, string
  local ok, err = api.load()
  if not ok then
    return nil, err
  end

  local full_id, resolve_err = data.resolve_id(_store as data.WorkStore, id)
  if not full_id then
    return nil, resolve_err
  end

  local item = data.get(_store as data.WorkStore, full_id) as WorkItem
  if not item then
    return nil, "item not found: " .. full_id
  end

  local timestamp = os.date("%Y-%m-%dT%H:%M:%S") as string
  data.add_log(item as data.WorkItem, message, timestamp)

  ok, err = data.save(item as data.WorkItem, _config.data_dir)
  if not ok then
    return nil, err
  end

  return timestamp
end

-- Set due date for item
-- Returns: item, err
api.set_due = function(id: string, due_date: string): WorkItem, string
  return api.update(id, { due = due_date })
end

-- Set blocks for item
-- blocks: array of item IDs or comma-separated string
-- Returns: item, err
api.set_blocks = function(id: string, blocks: {string} | string): WorkItem, string
  return api.update(id, { blocks = blocks })
end

-- Delete item
-- Returns: item (deleted), err
api.delete = function(id: string): WorkItem, string
  local ok, err = api.load()
  if not ok then
    return nil, err
  end

  local full_id, resolve_err = data.resolve_id(_store as data.WorkStore, id)
  if not full_id then
    return nil, resolve_err
  end

  local item = data.get(_store as data.WorkStore, full_id) as WorkItem
  if not item then
    return nil, "item not found: " .. full_id
  end

  -- Check for orphaned block references
  local referencing_items = process.find_items_blocking_on(_store, full_id) as {WorkItem}
  if #referencing_items > 0 then
    io.stderr:write("warning: deleting this item will orphan block references in:\n")
    for _, ref_item in ipairs(referencing_items) do
      local short_id = ref_item.id:sub(-6):lower()
      io.stderr:write(string.format("  - %s: %s\n", short_id, ref_item.title))
    end
    io.stderr:flush()
  end

  local clean_copy = data.clean(item as data.WorkItem) as WorkItem

  ok, err = data.delete(item as data.WorkItem, _config.data_dir)
  if not ok then
    return nil, err
  end

  -- Remove from store
  _store.items[full_id] = nil

  return clean_copy
end

-- Get file path for item
-- Returns: path, err
api.get_file_path = function(id: string): string, string
  local ok, err = api.load()
  if not ok then
    return nil, err
  end

  local full_id, resolve_err = data.resolve_id(_store as data.WorkStore, id)
  if not full_id then
    return nil, resolve_err
  end

  local item = data.get(_store as data.WorkStore, full_id) as WorkItem
  if not item then
    return nil, "item not found: " .. full_id
  end

  return item._meta and item._meta.source or path.join(_config.data_dir, full_id .. ".lua")
end

-- Clean item (remove internal fields)
-- Returns: cleaned item
api.clean = function(item: WorkItem): WorkItem
  return data.clean(item as data.WorkItem) as WorkItem
end

-- Expose render functions
api.render = render

return api
