local record ComputedFields
  short_id: string
  resolved_due: string
  relative_due: string
  is_blocked: boolean
  unresolved_blocks: {string}
  dependent_count: integer
end

local record WorkItemMeta
  source: string
  kind: string
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
  _meta: WorkItemMeta
  _computed: ComputedFields
end

local record WorkStore
  items: {string:WorkItem}
end

local record TreeResult
  roots: {WorkItem}
  children: {string:{WorkItem}}
end

local record M
  parse_date: function(date_str: string): integer, integer, integer, string
  date_to_days: function(year: integer, month: integer, day: integer): number
  days_to_date: function(days: number): string
  add_days_to_date: function(date_str: string, offset: integer): string, string
  parse_relative_date: function(rel_str: string): integer, string
  date_relative_to_today: function(date_str: string): string
  get_transitive_dependencies: function(store: WorkStore, item_id: string): {WorkItem}
  get_transitive_dependents: function(store: WorkStore, item_id: string): {WorkItem}
  validate_blocks: function(store: WorkStore, item_id: string, new_blocks: {string}): boolean, string
  resolve_due_date: function(store: WorkStore, item: WorkItem): string, string
  get_unresolved_blocks: function(store: WorkStore, item: WorkItem): {string}
  is_item_blocked: function(store: WorkStore, item: WorkItem): boolean
  get_blocked_items: function(store: WorkStore): {WorkItem}
  get_incomplete_items: function(store: WorkStore): {WorkItem}
  get_ready_items: function(store: WorkStore): {WorkItem}
  sort_by_schedule: function(items: {WorkItem}): {WorkItem}
  build_tree: function(items: {WorkItem}): TreeResult
  enrich: function(store: WorkStore, item: WorkItem): WorkItem
  enrich_all: function(store: WorkStore, items: {WorkItem}): {WorkItem}
  find_items_blocking_on: function(store: WorkStore, target_id: string): {WorkItem}
end

local process: M = {}

-- Parse date string YYYY-MM-DD into year, month, day
-- Returns: year, month, day or nil, err
process.parse_date = function(date_str: string): integer, integer, integer, string
  local year, month, day = date_str:match("^(%d%d%d%d)-(%d%d)-(%d%d)$")
  if not year then
    return nil, nil, nil, "invalid date format, expected YYYY-MM-DD"
  end
  return tonumber(year) as integer, tonumber(month) as integer, tonumber(day) as integer, nil
end

-- Convert date components to days since epoch
-- Returns: days since epoch
process.date_to_days = function(year: integer, month: integer, day: integer): number
  return os.time({year=year, month=month, day=day}) / 86400
end

-- Convert days since epoch to date string
-- Returns: date string YYYY-MM-DD
process.days_to_date = function(days: number): string
  local timestamp = days * 86400
  return os.date("%Y-%m-%d", timestamp) as string
end

-- Add offset to a date string
-- offset can be negative (e.g., -7 for 7 days before)
-- Returns: new date string or nil, err
process.add_days_to_date = function(date_str: string, offset: integer): string, string
  local year, month, day, err = process.parse_date(date_str)
  if not year then
    return nil, err or "invalid date format"
  end
  local days = process.date_to_days(year, month, day)
  return process.days_to_date(days + offset), nil
end

-- Parse relative date like "-1d" or "-4w"
-- Returns: offset in days (negative for before) or nil, err
process.parse_relative_date = function(rel_str: string): integer, string
  local sign, num, unit = rel_str:match("^([+-]?)(%d+)([dw])$")
  if not num then
    return nil, "invalid relative date format, expected -Xd or -Xw"
  end

  local num_val = tonumber(num) as integer
  if unit == "w" then
    num_val = num_val * 7
  end

  if sign == "+" then
    return num_val, nil
  else
    return -num_val, nil
  end
end

-- Calculate relative time from today to a date
-- Returns string like "+3w", "-2d", "today"
process.date_relative_to_today = function(date_str: string): string
  if not date_str then
    return nil
  end

  local year, month, day = process.parse_date(date_str)
  if not year then
    return nil
  end

  local target_days = process.date_to_days(year, month, day)
  local today = os.date("*t") as {string:integer}
  local today_days = process.date_to_days(today.year, today.month, today.day)

  local diff_days = target_days - today_days

  local abs_days = math.abs(diff_days)
  local sign: string
  if diff_days > 0 then
    sign = "+"
  elseif diff_days < 0 then
    sign = "-"
  else
    sign = " "
  end

  -- Use weeks for 14+ days, rounding up
  if abs_days >= 14 then
    local weeks = math.ceil(abs_days / 7)
    return sign .. tostring(weeks) .. "w"
  else
    return sign .. tostring(math.floor(abs_days)) .. "d"
  end
end

-- Get all items that this item transitively depends on
-- (items this blocks on, and items those block on, etc.)
-- Signature: M.get_transitive_dependencies(store, item_id)
-- Returns: array of items
process.get_transitive_dependencies = function(store: WorkStore, item_id: string): {WorkItem}
  local visited: {string:boolean} = {}
  local dependencies: {WorkItem} = {}

  local function visit(id: string)
    if visited[id] then
      return
    end
    visited[id] = true

    local item = store.items[id]
    if not item then
      return
    end

    table.insert(dependencies, item)

    if item.blocks then
      for _, block_id in ipairs(item.blocks) do
        visit(block_id)
      end
    end
  end

  visit(item_id)
  return dependencies
end

-- Get all items that transitively depend on this item
-- (items that block on this item, and items that block on those, etc.)
-- Signature: M.get_transitive_dependents(store, item_id)
-- Returns: array of items
process.get_transitive_dependents = function(store: WorkStore, item_id: string): {WorkItem}
  local visited: {string:boolean} = {}
  local dependents: {WorkItem} = {}

  local function visit(id: string)
    if visited[id] then
      return
    end
    visited[id] = true

    -- Find all items that block on this id
    for _, item in pairs(store.items) do
      if item.blocks then
        for _, block_id in ipairs(item.blocks) do
          if block_id == id then
            table.insert(dependents, item)
            visit(item.id)
            break
          end
        end
      end
    end
  end

  visit(item_id)
  return dependents
end

-- Validate that adding blocks doesn't create a cycle
-- Signature: M.validate_blocks(store, item_id, new_blocks)
-- Returns: ok, err
process.validate_blocks = function(store: WorkStore, item_id: string, new_blocks: {string}): boolean, string
  if not new_blocks or #new_blocks == 0 then
    return true, nil
  end

  -- Check for self-blocking
  for _, block_id in ipairs(new_blocks) do
    if block_id == item_id then
      return nil, "item cannot block on itself"
    end

    -- Check that the block reference exists (skip the item_id itself as it may not exist yet during add)
    if block_id ~= item_id and not store.items[block_id] then
      return nil, string.format("block reference '%s' does not exist", block_id)
    end
  end

  -- Check if this would create a cycle
  -- An item can't block on something that transitively blocks on it
  local dependents = process.get_transitive_dependents(store, item_id)

  for _, dependent in ipairs(dependents) do
    for _, block_id in ipairs(new_blocks) do
      if dependent.id == block_id then
        return nil, string.format("circular dependency: %s would block on %s, which depends on %s",
          item_id, block_id, item_id)
      end
    end
  end

  return true, nil
end

-- Resolve due date for an item
-- Returns: resolved_date, warning_message
-- resolved_date is nil if it couldn't be resolved
-- visiting is a table tracking items currently being resolved (for cycle detection)
-- items_table is the items collection to use
local function resolve_due_date_impl(item: WorkItem, visiting: {string:boolean}, items_table: {string:WorkItem}): string, string
  -- Cycle detection
  if visiting[item.id] then
    return nil, "circular dependency detected"
  end
  visiting[item.id] = true

  -- If no explicit due date, infer -1d from items this blocks on (if any)
  local offset: integer
  if not item.due then
    if item.blocks and #item.blocks > 0 then
      -- Infer -1d relative to what this blocks on
      offset = -1
    else
      -- No due date and doesn't block on anything
      visiting[item.id] = nil
      return nil, nil
    end
  else
    -- Check if it's a relative date
    local parsed_offset = process.parse_relative_date(item.due)
    if not parsed_offset then
      -- It's an absolute date, return as-is
      visiting[item.id] = nil
      return item.due, nil
    end
    offset = parsed_offset

    -- It's a relative date, compute based on items this blocks on (transitively)
    if not item.blocks or #item.blocks == 0 then
      visiting[item.id] = nil
      return nil, "relative due date '" .. item.due .. "' but this item doesn't block on anything"
    end
  end

  -- Find earliest due date among all transitive blocking items
  -- (items this blocks on, items those block on, etc.)
  local earliest_date: string = nil
  local earliest_days: number = nil
  local has_any_due = false

  local function collect_blocking_dates(id: string, seen: {string:boolean})
    if seen[id] then
      return
    end
    seen[id] = true

    local blocking_item = items_table[id]
    if not blocking_item then
      io.stderr:write(string.format("warning: blocking item %s not found\n", id))
      return
    end

    -- Get this item's due date
    local block_date, _ = resolve_due_date_impl(blocking_item, visiting, items_table)
    if block_date then
      has_any_due = true
      local year, month, day = process.parse_date(block_date)
      if year then
        local days = process.date_to_days(year, month, day)
        if not earliest_days or days < earliest_days then
          earliest_days = days
          earliest_date = block_date
        end
      end
    end

    -- Recursively check what this item blocks on
    if blocking_item.blocks then
      for _, transitive_block_id in ipairs(blocking_item.blocks) do
        collect_blocking_dates(transitive_block_id, seen)
      end
    end
  end

  -- Collect dates from all transitive blocking items
  local seen: {string:boolean} = {}
  for _, block_id in ipairs(item.blocks) do
    collect_blocking_dates(block_id, seen)
  end

  visiting[item.id] = nil

  if not has_any_due then
    if item.due then
      return nil, "relative due date '" .. item.due .. "' but no blocking items have due dates"
    else
      return nil, "cannot infer due date: no blocking items have due dates"
    end
  end

  -- Apply offset to earliest date
  local resolved = process.add_days_to_date(earliest_date, offset)
  return resolved, nil
end

-- Resolve due date for an item
-- Signature: M.resolve_due_date(store, item)
-- Returns: resolved_date, warning_message
process.resolve_due_date = function(store: WorkStore, item: WorkItem): string, string
  return resolve_due_date_impl(item, {}, store.items)
end

-- Get unresolved blocks for an item
-- Returns items that are blocking this item (items that have this item in their blocks field and are not completed)
-- Signature: M.get_unresolved_blocks(store, item)
-- Returns: array of IDs
process.get_unresolved_blocks = function(store: WorkStore, item: WorkItem): {string}
  local unresolved: {string} = {}
  -- Find all incomplete items that block on this item
  for _, other in pairs(store.items) do
    if not other.completed and other.blocks then
      for _, blocked_id in ipairs(other.blocks) do
        if blocked_id == item.id then
          table.insert(unresolved, other.id)
          break
        end
      end
    end
  end
  return unresolved
end

-- Check if an item is blocked
-- Signature: M.is_item_blocked(store, item)
-- Returns: boolean
process.is_item_blocked = function(store: WorkStore, item: WorkItem): boolean
  -- An item is blocked if any other incomplete item blocks on this item
  -- (meaning this item must wait for those items to complete first)
  for _, other in pairs(store.items) do
    if not other.completed and other.blocks then
      for _, blocked_id in ipairs(other.blocks) do
        if blocked_id == item.id then
          return true
        end
      end
    end
  end
  return false
end

-- Get blocked items (items that are blocked by other incomplete items)
-- Signature: M.get_blocked_items(store)
-- Returns: array of blocked items
process.get_blocked_items = function(store: WorkStore): {WorkItem}
  local blocked: {WorkItem} = {}
  for _, item in pairs(store.items) do
    if not item.completed then
      if process.is_item_blocked(store, item) then
        table.insert(blocked, item)
      end
    end
  end
  return blocked
end

-- Get all incomplete items
-- Signature: M.get_incomplete_items(store)
-- Returns items that are not completed
process.get_incomplete_items = function(store: WorkStore): {WorkItem}
  local incomplete: {WorkItem} = {}
  for _, item in pairs(store.items) do
    if not item.completed then
      table.insert(incomplete, item)
    end
  end
  return incomplete
end

-- Get ready (unblocked) items
-- Signature: M.get_ready_items(store)
-- Returns items that are not completed and have no incomplete blockers
process.get_ready_items = function(store: WorkStore): {WorkItem}
  local ready: {WorkItem} = {}
  for _, item in pairs(store.items) do
    -- Skip completed items
    if not item.completed then
      if not process.is_item_blocked(store, item) then
        table.insert(ready, item)
      end
    end
  end
  return ready
end

-- Sort items by due date (ascending), priority (descending), created (ascending), id (ascending)
-- Returns: sorted items (in-place)
process.sort_by_schedule = function(items: {WorkItem}): {WorkItem}
  table.sort(items, function(a: WorkItem, b: WorkItem): boolean
    -- Use pre-computed due date if available (from enrich), otherwise nil
    local a_due: string = (a._computed and a._computed.resolved_due) or nil
    local b_due: string = (b._computed and b._computed.resolved_due) or nil

    -- Items with due dates come before items without
    if a_due and not b_due then
      return true
    elseif not a_due and b_due then
      return false
    elseif a_due and b_due then
      -- Both have due dates - compare them (ascending: earliest first)
      if a_due ~= b_due then
        -- Convert to days for proper numeric comparison
        local a_year, a_month, a_day = process.parse_date(a_due)
        local b_year, b_month, b_day = process.parse_date(b_due)
        if a_year and b_year then
          local a_days = process.date_to_days(a_year, a_month, a_day)
          local b_days = process.date_to_days(b_year, b_month, b_day)
          return a_days < b_days
        end
        -- Fallback to string comparison if parsing fails
        return a_due < b_due
      end
    end

    -- Same due date (or both have no due date) - sort by priority (descending)
    local a_priority = a.priority or 0
    local b_priority = b.priority or 0
    if a_priority ~= b_priority then
      return a_priority > b_priority
    end

    -- Same priority - sort by creation date (ascending)
    if (a.created or "") ~= (b.created or "") then
      return (a.created or "") < (b.created or "")
    end

    -- Same creation date - sort by ID (ascending) for stable sort
    -- ULIDs encode creation time, so this maintains chronological order
    return a.id < b.id
  end)
  return items
end

-- Build a tree structure from items
-- Returns: {roots = array, children = map}
process.build_tree = function(items: {WorkItem}): TreeResult
  local children: {string:{WorkItem}} = {}
  local is_child: {string:boolean} = {}
  local item_set: {string:boolean} = {}

  -- Build set of items for quick lookup
  for _, item in ipairs(items) do
    item_set[item.id] = true
  end

  -- Build children map
  for _, item in ipairs(items) do
    if item.blocks then
      for _, block_id in ipairs(item.blocks) do
        -- Only consider it a child if the parent is in our filtered set
        if item_set[block_id] then
          if not children[block_id] then
            children[block_id] = {}
          end
          table.insert(children[block_id], item)
          is_child[item.id] = true
        end
      end
    end
  end

  -- Find roots (items that are not children in this filtered set)
  local roots: {WorkItem} = {}
  for _, item in ipairs(items) do
    if not is_child[item.id] then
      table.insert(roots, item)
    end
  end

  return {
    roots = roots,
    children = children,
  }
end

-- Enrich an item with computed fields
-- Signature: M.enrich(store, item)
-- Returns: item with _computed field
process.enrich = function(store: WorkStore, item: WorkItem): WorkItem
  -- Don't re-enrich
  if item._computed then
    return item
  end

  -- Calculate short ID (last 6 characters)
  local short_id = item.id:sub(-6)

  -- Resolve due date
  local resolved_due, _ = process.resolve_due_date(store, item)
  local relative_due = process.date_relative_to_today(resolved_due)

  -- Check if blocked
  local is_blocked = process.is_item_blocked(store, item)

  -- Get unresolved blocks
  local unresolved_blocks = process.get_unresolved_blocks(store, item)

  -- Count dependents (items that block on this one)
  local dependent_count = 0
  for _, other in pairs(store.items) do
    if other.blocks then
      for _, block_id in ipairs(other.blocks) do
        if block_id == item.id then
          dependent_count = dependent_count + 1
          break
        end
      end
    end
  end

  item._computed = {
    short_id = short_id,
    resolved_due = resolved_due,
    relative_due = relative_due,
    is_blocked = is_blocked,
    unresolved_blocks = unresolved_blocks,
    dependent_count = dependent_count,
  }

  return item
end

-- Enrich all items with computed fields
-- Signature: M.enrich_all(store, items)
-- Returns: array of enriched items
process.enrich_all = function(store: WorkStore, items: {WorkItem}): {WorkItem}
  local enriched: {WorkItem} = {}
  for _, item in ipairs(items) do
    table.insert(enriched, process.enrich(store, item))
  end
  return enriched
end

-- Find items that have the given ID in their blocks field
-- Signature: M.find_items_blocking_on(store, target_id)
-- Returns: array of items that reference this ID
process.find_items_blocking_on = function(store: WorkStore, target_id: string): {WorkItem}
  local referencing_items: {WorkItem} = {}
  for _, item in pairs(store.items) do
    if item.blocks then
      for _, block_id in ipairs(item.blocks) do
        if block_id == target_id then
          table.insert(referencing_items, item)
          break
        end
      end
    end
  end
  return referencing_items
end

return process
