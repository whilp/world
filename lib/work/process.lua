local M = {}

-- Parse date string YYYY-MM-DD into year, month, day
-- Returns: year, month, day or nil, err
M.parse_date = function(date_str)
  local year, month, day = date_str:match("^(%d%d%d%d)-(%d%d)-(%d%d)$")
  if not year then
    return nil, "invalid date format, expected YYYY-MM-DD"
  end
  return tonumber(year), tonumber(month), tonumber(day)
end

-- Convert date components to days since epoch
-- Returns: days since epoch
M.date_to_days = function(year, month, day)
  return os.time({year=year, month=month, day=day}) / 86400
end

-- Convert days since epoch to date string
-- Returns: date string YYYY-MM-DD
M.days_to_date = function(days)
  local timestamp = days * 86400
  return os.date("%Y-%m-%d", timestamp)
end

-- Add offset to a date string
-- offset can be negative (e.g., -7 for 7 days before)
-- Returns: new date string or nil, err
M.add_days_to_date = function(date_str, offset)
  local year, month, day = M.parse_date(date_str)
  if not year then
    return nil, "invalid date format"
  end
  local days = M.date_to_days(year, month, day)
  return M.days_to_date(days + offset)
end

-- Parse relative date like "-1d" or "-4w"
-- Returns: offset in days (negative for before) or nil, err
M.parse_relative_date = function(rel_str)
  local sign, num, unit = rel_str:match("^([+-]?)(%d+)([dw])$")
  if not num then
    return nil, "invalid relative date format, expected -Xd or -Xw"
  end

  num = tonumber(num)
  if unit == "w" then
    num = num * 7
  end

  if sign == "+" then
    return num
  else
    return -num
  end
end

-- Calculate relative time from today to a date
-- Returns string like "+3w", "-2d", "today"
M.date_relative_to_today = function(date_str)
  if not date_str then
    return nil
  end

  local year, month, day = M.parse_date(date_str)
  if not year then
    return nil
  end

  local target_days = M.date_to_days(year, month, day)
  local today = os.date("*t")
  local today_days = M.date_to_days(today.year, today.month, today.day)

  local diff_days = target_days - today_days

  local abs_days = math.abs(diff_days)
  local sign = diff_days > 0 and "+" or ""
  if diff_days < 0 then
    sign = "-"
  elseif diff_days == 0 then
    sign = " "
  end

  -- Use weeks for 14+ days, rounding up
  if abs_days >= 14 then
    local weeks = math.ceil(abs_days / 7)
    return sign .. tostring(weeks) .. "w"
  else
    return sign .. tostring(abs_days) .. "d"
  end
end

-- Get all items that this item transitively depends on
-- (items this blocks on, and items those block on, etc.)
-- Signature: M.get_transitive_dependencies(store, item_id)
-- Returns: array of items
M.get_transitive_dependencies = function(store, item_id)
  local visited = {}
  local dependencies = {}

  local function visit(id)
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
M.get_transitive_dependents = function(store, item_id)
  local visited = {}
  local dependents = {}

  local function visit(id)
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
M.validate_blocks = function(store, item_id, new_blocks)
  if not new_blocks or #new_blocks == 0 then
    return true
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
  local dependents = M.get_transitive_dependents(store, item_id)

  for _, dependent in ipairs(dependents) do
    for _, block_id in ipairs(new_blocks) do
      if dependent.id == block_id then
        return nil, string.format("circular dependency: %s would block on %s, which depends on %s",
          item_id, block_id, item_id)
      end
    end
  end

  return true
end

-- Resolve due date for an item
-- Returns: resolved_date, warning_message
-- resolved_date is nil if it couldn't be resolved
-- visiting is a table tracking items currently being resolved (for cycle detection)
-- items_table is the items collection to use
local function resolve_due_date_impl(item, visiting, items_table)
  -- Cycle detection
  if visiting[item.id] then
    return nil, "circular dependency detected"
  end
  visiting[item.id] = true

  -- If no explicit due date, infer -1d from items this blocks on (if any)
  local offset
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
    offset = M.parse_relative_date(item.due)
    if not offset then
      -- It's an absolute date, return as-is
      visiting[item.id] = nil
      return item.due, nil
    end

    -- It's a relative date, compute based on items this blocks on (transitively)
    if not item.blocks or #item.blocks == 0 then
      visiting[item.id] = nil
      return nil, "relative due date '" .. item.due .. "' but this item doesn't block on anything"
    end
  end

  -- Find earliest due date among all transitive blocking items
  -- (items this blocks on, items those block on, etc.)
  local earliest_date = nil
  local earliest_days = nil
  local has_any_due = false

  local function collect_blocking_dates(id, seen)
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
      local year, month, day = M.parse_date(block_date)
      if year then
        local days = M.date_to_days(year, month, day)
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
  local seen = {}
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
  local resolved = M.add_days_to_date(earliest_date, offset)
  return resolved, nil
end

-- Resolve due date for an item
-- Signature: M.resolve_due_date(store, item)
-- Returns: resolved_date, warning_message
M.resolve_due_date = function(store, item)
  return resolve_due_date_impl(item, {}, store.items)
end

-- Get unresolved blocks for an item
-- Returns items that are blocking this item (items that have this item in their blocks field and are not completed)
-- Signature: M.get_unresolved_blocks(store, item)
-- Returns: array of IDs
M.get_unresolved_blocks = function(store, item)
  local unresolved = {}
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
M.is_item_blocked = function(store, item)
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
M.get_blocked_items = function(store)
  local blocked = {}
  for _, item in pairs(store.items) do
    if not item.completed then
      if M.is_item_blocked(store, item) then
        table.insert(blocked, item)
      end
    end
  end
  return blocked
end

-- Get all incomplete items
-- Signature: M.get_incomplete_items(store)
-- Returns items that are not completed
M.get_incomplete_items = function(store)
  local incomplete = {}
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
M.get_ready_items = function(store)
  local ready = {}
  for _, item in pairs(store.items) do
    -- Skip completed items
    if not item.completed then
      if not M.is_item_blocked(store, item) then
        table.insert(ready, item)
      end
    end
  end
  return ready
end

-- Sort items by due date (ascending), priority (descending), created (ascending), id (ascending)
-- Returns: sorted items (in-place)
M.sort_by_schedule = function(items)
  table.sort(items, function(a, b)
    -- Use pre-computed due date if available (from enrich), otherwise nil
    local a_due = (a._computed and a._computed.resolved_due) or nil
    local b_due = (b._computed and b._computed.resolved_due) or nil

    -- Items with due dates come before items without
    if a_due and not b_due then
      return true
    elseif not a_due and b_due then
      return false
    elseif a_due and b_due then
      -- Both have due dates - compare them (ascending: earliest first)
      if a_due ~= b_due then
        -- Convert to days for proper numeric comparison
        local a_year, a_month, a_day = M.parse_date(a_due)
        local b_year, b_month, b_day = M.parse_date(b_due)
        if a_year and b_year then
          local a_days = M.date_to_days(a_year, a_month, a_day)
          local b_days = M.date_to_days(b_year, b_month, b_day)
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
M.build_tree = function(items)
  local children = {}
  local is_child = {}
  local item_set = {}

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
  local roots = {}
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
M.enrich = function(store, item)
  -- Don't re-enrich
  if item._computed then
    return item
  end

  -- Calculate short ID (last 6 characters)
  local short_id = item.id:sub(-6)

  -- Resolve due date
  local resolved_due, _ = M.resolve_due_date(store, item)
  local relative_due = M.date_relative_to_today(resolved_due)

  -- Check if blocked
  local is_blocked = M.is_item_blocked(store, item)

  -- Get unresolved blocks
  local unresolved_blocks = M.get_unresolved_blocks(store, item)

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
M.enrich_all = function(store, items)
  local enriched = {}
  for _, item in ipairs(items) do
    table.insert(enriched, M.enrich(store, item))
  end
  return enriched
end

-- Find items that have the given ID in their blocks field
-- Signature: M.find_items_blocking_on(store, target_id)
-- Returns: array of items that reference this ID
M.find_items_blocking_on = function(store, target_id)
  local referencing_items = {}
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

return M
