local path = require("cosmo.path")

-- Record type for work item metadata
local record WorkItemMeta
  source: string
  kind: string
end

-- Record type for work items
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

-- Record type for the work store
local record WorkStore
  items: {string:WorkItem}
end

-- Record type for dangerous pattern checks
local record DangerousPattern
  pattern: string
  desc: string
end

-- Record type for render options
local record RenderOpts
  format: string
  exclude: {string}
  indent: string
end

-- Callback type for work item handlers
local type WorkCallback = function(item: WorkItem)

-- Record type for kind callbacks
local type WorkKinds = {string:WorkCallback}

local record M
  _lock_handle: number
  _lock_path: string

  validate_string_content: function(str: string, field_name: string): boolean, string
  validate: function(item: WorkItem): boolean, string
  load_file: function(file_path: string, store: WorkStore, kinds?: WorkKinds): boolean, string
  clean: function(item: WorkItem): WorkItem
  render: function(data: {string:any}, opts?: RenderOpts): string
  write: function(item: WorkItem, callback_name?: string): string
  get: function(store: WorkStore, id: string): WorkItem
  get_all: function(store: WorkStore): {WorkItem}
  get_by_file: function(store: WorkStore, source: string): {WorkItem}
  acquire_lock: function(dir: string): boolean, string
  release_lock: function(): boolean, string
  load_all: function(store: WorkStore, dir: string): boolean, string
  save: function(item: WorkItem, dir: string): boolean, string
  list_backups: function(dir: string): {string}
  restore_backup: function(backup_path: string): boolean, string
  delete: function(item: WorkItem, dir?: string): boolean, string
  generate_id: function(): string
  resolve_id: function(store: WorkStore, short_id: string): string, string
  add_log: function(item: WorkItem, message: string, timestamp?: string): WorkItem
  mark_done: function(item: WorkItem, timestamp?: string): WorkItem
  mark_started: function(item: WorkItem, timestamp?: string): WorkItem
end

local data: M = {
  _lock_handle = nil,
  _lock_path = nil,
}

-- Validate string content for dangerous lua patterns
data.validate_string_content = function(str: string, field_name: string): boolean, string
  if type(str) ~= "string" then
    return nil, string.format("field '%s' must be string, got %s", field_name, type(str))
  end

  local dangerous_patterns: {DangerousPattern} = {
    { pattern = "]]", desc = "breaks long string delimiters" },
    { pattern = "%-%-%[%[", desc = "creates multiline comment" },
    { pattern = "%-%-%[=", desc = "creates multiline comment with level" },
    { pattern = "%[=%[", desc = "nested long string delimiter" },
    { pattern = "%[==%[", desc = "nested long string delimiter" },
    { pattern = "\0", desc = "null byte" },
  }

  for _, check in ipairs(dangerous_patterns) do
    if str:find(check.pattern) then
      return nil, string.format(
        "field '%s' contains dangerous pattern '%s' (%s)",
        field_name,
        check.pattern:gsub("%%", ""),
        check.desc
      )
    end
  end

  return true
end

-- Validate item schema
data.validate = function(item: WorkItem): boolean, string
  local function type_check(value: any, expected_type: string, field_name: string): boolean, string
    local actual_type = type(value)
    if actual_type ~= expected_type then
      return nil, string.format("field '%s' must be %s, got %s", field_name, expected_type, actual_type)
    end
    return true
  end

  if not item.id then
    return nil, "missing required field 'id'"
  end
  local ok, err = type_check(item.id, "string", "id")
  if not ok then return nil, err end

  if not item.title then
    return nil, "missing required field 'title'"
  end
  ok, err = type_check(item.title, "string", "title")
  if not ok then return nil, err end
  ok, err = data.validate_string_content(item.title, "title")
  if not ok then return nil, err end

  if item.created then
    ok, err = type_check(item.created, "string", "created")
    if not ok then return nil, err end
  end

  if item.blocks then
    ok, err = type_check(item.blocks, "table", "blocks")
    if not ok then return nil, err end
    for i, block_id in ipairs(item.blocks) do
      if type(block_id) ~= "string" then
        return nil, string.format("blocks[%d] must be string, got %s", i, type(block_id))
      end
    end
  end

  if item.completed and type(item.completed) ~= "string" then
    return nil, string.format("field 'completed' must be string, got %s", type(item.completed))
  end

  if item.started and type(item.started) ~= "string" then
    return nil, string.format("field 'started' must be string, got %s", type(item.started))
  end

  if item.description then
    if type(item.description) ~= "string" then
      return nil, string.format("field 'description' must be string, got %s", type(item.description))
    end
    ok, err = data.validate_string_content(item.description, "description")
    if not ok then return nil, err end
  end

  if item.due and type(item.due) ~= "string" then
    return nil, string.format("field 'due' must be string, got %s", type(item.due))
  end

  if item.priority and type(item.priority) ~= "number" then
    return nil, string.format("field 'priority' must be number, got %s", type(item.priority))
  end

  if item.log then
    ok, err = type_check(item.log, "table", "log")
    if not ok then return nil, err end
    for timestamp, message in pairs(item.log) do
      if type(timestamp) ~= "string" then
        return nil, string.format("log keys must be string timestamps, got %s", type(timestamp))
      end
      if type(message) ~= "string" then
        return nil, string.format("log['%s'] must be string, got %s", timestamp, type(message))
      end
      ok, err = data.validate_string_content(message, string.format("log['%s']", timestamp))
      if not ok then return nil, err end
    end
  end

  return true
end

-- Create default Work{} callback that adds items to a specific items table
local function make_work_callback(items_table: {string:WorkItem}): WorkCallback
  return function(item: WorkItem)
    local source = item._meta and item._meta.source or "unknown"

    local ok, err = data.validate(item)
    if not ok then
      error("item validation failed in " .. source .. ": " .. err)
    end

    if items_table[item.id] then
      local existing = items_table[item.id]
      local existing_path = existing._meta and existing._meta.source or "unknown"
      error("item id collision: " .. item.id .. " (existing: " .. existing_path .. ", new: " .. source .. ")")
    end

    items_table[item.id] = item
  end
end

-- Load and execute lua file with Work{} callbacks
data.load_file = function(file_path: string, store: WorkStore, kinds?: WorkKinds): boolean, string
  local env: WorkKinds = kinds or {}

  if not env.Work then
    env.Work = make_work_callback(store.items)
  end

  local wrapped_env: {string:WorkCallback} = {}
  for kind_name, callback in pairs(env) do
    wrapped_env[kind_name] = function(item: WorkItem)
      if not item._meta then
        item._meta = {} as WorkItemMeta
      end
      item._meta.source = debug.getinfo(2, "S").source:sub(2)
      item._meta.kind = kind_name
      callback(item)
    end
  end

  local loader, err = loadfile(file_path, "t", wrapped_env as {string:any})
  if not loader then
    return false, err
  end

  local ok, load_err = pcall(loader)

  return ok, load_err as string
end

-- Clean internal fields (_meta, _computed)
data.clean = function(item: WorkItem): WorkItem
  local clean: WorkItem = {} as WorkItem
  for k, v in pairs(item as {string:any}) do
    if not k:match("^_") then
      (clean as {string:any})[k] = v
    end
  end
  return clean
end

-- Render data structure to lua format
data.render = function(input_data: {string:any}, opts?: RenderOpts): string
  opts = opts or {} as RenderOpts
  local format = opts.format or "serpent"
  local exclude = opts.exclude or {}

  local filtered: {string:any} = {}
  for k, v in pairs(input_data) do
    local skip = false
    for _, excl in ipairs(exclude) do
      if k == excl then
        skip = true
        break
      end
    end
    if not skip then
      filtered[k] = v
    end
  end

  if format == "serpent" then
    local serpent = require("serpent")
    return "return " .. serpent.block(filtered, {
      comment = false,
      sortkeys = true,
      indent = opts.indent or "  ",
    }) .. "\n"
  else
    return tostring(filtered)
  end
end

-- Serialize item to Work{} format
data.write = function(item: WorkItem, callback_name?: string): string
  callback_name = callback_name or (item._meta and item._meta.kind) or "Work"

  local clean = data.clean(item)
  local rendered = data.render(clean as {string:any})
  rendered = rendered:gsub("^return ", callback_name)
  return rendered
end

-- Get item by full ID
data.get = function(store: WorkStore, id: string): WorkItem
  return store.items[id]
end

-- Get all items sorted by created date
data.get_all = function(store: WorkStore): {WorkItem}
  local all: {WorkItem} = {}
  for _, item in pairs(store.items) do
    table.insert(all, item)
  end
  table.sort(all, function(a: WorkItem, b: WorkItem): boolean
    return (a.created or "") < (b.created or "")
  end)
  return all
end

-- Get items from specific file
data.get_by_file = function(store: WorkStore, source: string): {WorkItem}
  local items: {WorkItem} = {}
  for _, item in pairs(store.items) do
    if item._meta and item._meta.source == source then
      table.insert(items, item)
    end
  end
  return items
end

-- Ensure data directory exists
local function ensure_data_dir(dir: string): boolean, string
  local posix = require("posix")
  local _, err = posix.mkdir(dir)
  if err and not err:match("File exists") then
    return nil, "failed to create data directory: " .. dir .. ": " .. tostring(err)
  end
  return true
end

-- Acquire exclusive lock on data directory
data.acquire_lock = function(dir: string): boolean, string
  local ok, err = ensure_data_dir(dir)
  if not ok then
    return nil, err
  end

  local fcntl = require("posix.fcntl")
  local lock_path = path.join(dir, ".work.lock")

  -- Open or create lock file
  local fd: number
  fd, err = fcntl.open(lock_path, fcntl.O_CREAT + fcntl.O_RDWR, tonumber("0600", 8))
  if not fd then
    return nil, "failed to open lock file: " .. tostring(err)
  end

  -- Try to acquire exclusive lock (non-blocking)
  local lock = {
    l_type = fcntl.F_WRLCK,
    l_whence = fcntl.SEEK_SET,
    l_start = 0,
    l_len = 0,
  }

  ok = fcntl.fcntl(fd, fcntl.F_SETLK, lock)
  if not ok then
    local unistd = require("posix.unistd")
    unistd.close(fd)
    return nil, "another process is modifying work items"
  end

  -- Store lock handle and path for cleanup
  data._lock_handle = fd
  data._lock_path = lock_path

  return true
end

-- Release lock
data.release_lock = function(): boolean, string
  if not data._lock_handle then
    return true
  end

  local unistd = require("posix.unistd")

  -- Release lock by closing file descriptor
  unistd.close(data._lock_handle)
  data._lock_handle = nil
  data._lock_path = nil

  return true
end

-- Load all work items from directory
data.load_all = function(store: WorkStore, dir: string): boolean, string
  local ok, err = ensure_data_dir(dir)
  if not ok then
    return nil, err
  end

  local glob = require("posix.glob")
  local pattern = path.join(dir, "*.lua")
  local files = glob.glob(pattern, 0)

  if not files or #files == 0 then
    return true  -- no files is ok
  end

  for _, file in ipairs(files) do
    ok, err = data.load_file(file, store)
    if not ok then
      return nil, "failed to load " .. file .. ": " .. tostring(err)
    end
  end
  return true
end

-- Signal handling for cleanup
local signal = require("posix.signal")
local cleanup_registry: {string} = {}

local function cleanup_temp_files()
  for _, temp_path in ipairs(cleanup_registry) do
    if temp_path then
      os.remove(temp_path)
    end
  end
end

local function setup_signal_handlers()
  local handler = function(signum: number)
    cleanup_temp_files()
    os.exit(128 + signum as integer)
  end
  signal.signal(signal.SIGINT, handler)
  signal.signal(signal.SIGTERM, handler)
end

setup_signal_handlers()

-- Save item (validates, atomic write)
data.save = function(item: WorkItem, dir: string): boolean, string
  -- Acquire lock
  local ok, err = data.acquire_lock(dir)
  if not ok then
    return nil, err
  end

  ok, err = ensure_data_dir(dir)
  if not ok then
    data.release_lock()
    return nil, err
  end

  ok, err = data.validate(item)
  if not ok then
    data.release_lock()
    return nil, err
  end

  local source = item._meta and item._meta.source
  if not source then
    source = path.join(dir, item.id .. ".lua")
  end

  -- Create backup if file already exists
  local backup_path = source .. ".bak"
  local existing_file = io.open(source, "r")
  if existing_file then
    local content = existing_file:read("*a")
    existing_file:close()

    local backup_file = io.open(backup_path, "w")
    if not backup_file then
      data.release_lock()
      return nil, "failed to create backup file: " .. backup_path
    end
    backup_file:write(content)
    backup_file:close()
  end

  -- Atomic write pattern: temp file in same directory for atomic rename
  local unistd = require("posix.unistd")
  local stdlib = require("posix.stdlib")
  local fd: number
  local temp: string
  fd, temp = stdlib.mkstemp(source .. ".tmp.XXXXXX")
  if not fd then
    data.release_lock()
    return nil, "failed to create temp file: " .. tostring(temp)
  end
  unistd.close(fd)  -- close fd, reopen with io.open for simpler API

  -- Register temp file for signal handler cleanup
  table.insert(cleanup_registry, temp)

  local f = io.open(temp, "w")
  if not f then
    cleanup_registry[#cleanup_registry] = nil  -- remove from registry
    data.release_lock()
    return nil, "failed to open temp file for writing: " .. temp
  end
  f:write(data.write(item))
  f:close()

  ok, err = os.rename(temp, source)
  if not ok then
    os.remove(temp)  -- cleanup on failure
    cleanup_registry[#cleanup_registry] = nil  -- remove from registry
    data.release_lock()
    return nil, "failed to rename temp file: " .. tostring(err)
  end

  -- Remove from registry after successful rename
  cleanup_registry[#cleanup_registry] = nil

  -- Remove backup file after successful save
  if existing_file then
    os.remove(backup_path)
  end

  -- Release lock
  data.release_lock()
  return true
end

-- List all backup files in directory
data.list_backups = function(dir: string): {string}
  local glob = require("posix.glob")
  local pattern = path.join(dir, "*.lua.bak")
  local files = glob.glob(pattern, 0)
  return files or {}
end

-- Restore backup file to original
data.restore_backup = function(backup_path: string): boolean, string
  if not backup_path:match("%.bak$") then
    return nil, "backup path must end with .bak"
  end

  local original_path = backup_path:sub(1, -5)  -- remove .bak suffix

  local ok, err = os.rename(backup_path, original_path)
  if not ok then
    return nil, "failed to restore backup: " .. tostring(err)
  end

  return true
end

-- Delete item file
data.delete = function(item: WorkItem, dir?: string): boolean, string
  local source = item._meta and item._meta.source
  if not source then
    return true
  end

  -- Extract directory from source if dir not provided
  if not dir then
    dir = source:match("(.*/)[^/]+$") or "."
    -- Remove trailing slash
    dir = dir:gsub("/$", "")
  end

  -- Acquire lock
  local ok, err = data.acquire_lock(dir)
  if not ok then
    return nil, err
  end

  ok = os.remove(source)
  if not ok then
    data.release_lock()
    return nil, "failed to remove file: " .. source
  end

  data.release_lock()
  return true
end

-- Generate new ULID
data.generate_id = function(): string
  local ulid = require("ulid")
  return ulid.generate()
end

-- Resolve short ID to full ID
data.resolve_id = function(store: WorkStore, short_id: string): string, string
  -- Validate input
  if not short_id or short_id == "" then
    return nil, "id cannot be empty"
  end

  -- Normalize to uppercase for comparison (ULIDs are stored uppercase)
  local normalized = short_id:upper()

  -- If it's already a full ID, return it
  if #normalized >= 26 then
    return normalized
  end

  -- Find all items that match the prefix or suffix
  local matches: {string} = {}
  for id, _ in pairs(store.items) do
    if id:sub(1, #normalized) == normalized or id:sub(-#normalized) == normalized then
      table.insert(matches, id)
    end
  end

  if #matches == 0 then
    return nil, "no work item found matching: " .. short_id
  elseif #matches > 1 then
    return nil, string.format("ambiguous id '%s', matches: %s", short_id, table.concat(matches, ", "))
  end

  return matches[1]  -- explicit success return
end

-- Add log entry to item with timestamp
data.add_log = function(item: WorkItem, message: string, timestamp?: string): WorkItem
  timestamp = timestamp or os.date("%Y-%m-%dT%H:%M:%S") as string
  item.log = item.log or {}
  item.log[timestamp] = message
  return item
end

-- Mark item as complete with timestamp
data.mark_done = function(item: WorkItem, timestamp?: string): WorkItem
  timestamp = timestamp or os.date("!%Y-%m-%dT%H:%M:%S") as string
  item.completed = timestamp
  return item
end

-- Mark item as started with timestamp
data.mark_started = function(item: WorkItem, timestamp?: string): WorkItem
  timestamp = timestamp or os.date("!%Y-%m-%dT%H:%M:%S") as string
  item.started = timestamp
  return item
end

return data
