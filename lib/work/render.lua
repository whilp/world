local M = {}

-- Helper: get work library for date functions
local work = require("work.process")

-- Component functions (building blocks)

-- Returns: "○" for incomplete, "◉" for completed
M.status_mark = function(item)
  return item.completed and "◉" or "○"
end

-- Returns: last 6 chars from item._computed.short_id or item.id (lowercase)
M.short_id = function(item)
  if item._computed and item._computed.short_id then
    return item._computed.short_id:lower()
  end
  return item.id:sub(-6):lower()
end

-- Returns: 5-char string like "+3w ", " +3d ", "  ?  ", or "     "
M.due_display = function(item)
  -- Use pre-computed resolved_due from enriched item
  local resolved_due = item._computed and item._computed.resolved_due
  if resolved_due then
    local relative = work.date_relative_to_today(resolved_due)
    if relative then
      -- Pad to 4 chars (e.g., "+13w", "+12d", " +3d"), then add trailing space
      return string.format("%4s", relative) .. " "
    end
  elseif item.due then
    -- Has explicit due but couldn't resolve
    return "  ?  "
  end
  -- No due date
  return "     "
end

-- Returns: " [p1]" or ""
M.priority_badge = function(item)
  if item.priority and item.priority ~= 0 then
    return string.format(" [p%d]", item.priority)
  end
  return ""
end

-- Returns: " (blocks: X, Y)" or ""
M.blocks_info = function(item)
  if not item.blocks or #item.blocks == 0 then
    return ""
  end
  local short_blocks = {}
  for _, block_id in ipairs(item.blocks) do
    table.insert(short_blocks, block_id:sub(-6):lower())
  end
  return string.format(" (blocks: %s)", table.concat(short_blocks, ", "))
end

-- Format functions (complete outputs)

-- Returns: multi-line string in list format
-- Example:
-- ○ 2F2J: title (blocks: X, Y)
-- ◉ 3K4M: completed task
M.list = function(items, opts)
  opts = opts or {}

  if #items == 0 then
    return "no work items"
  end

  local lines = {}
  for _, item in ipairs(items) do
    local line = string.format("%s %s: %s%s",
      M.status_mark(item),
      M.short_id(item),
      item.title,
      M.blocks_info(item))
    table.insert(lines, line)
  end

  return table.concat(lines, "\n")
end

-- Returns: multi-line string in tree format
-- Example:
-- ○ 2F2J +3w  title
--     ○ 4K5N +1d  subtask
-- ◉ 6M7P      completed root
--
-- tree_data format: {roots = array, children = map}
M.tree = function(tree_data, opts)
  opts = opts or {}

  local roots = tree_data.roots or {}
  local children = tree_data.children or {}

  if #roots == 0 then
    return "no work items"
  end

  local lines = {}

  local function print_tree(item, indent)
    local line = string.format("%s%s %s %s%s",
      indent,
      M.status_mark(item),
      M.short_id(item),
      M.due_display(item),
      item.title)
    table.insert(lines, line)

    -- Print children
    if children[item.id] then
      for _, child in ipairs(children[item.id]) do
        print_tree(child, indent .. "    ")
      end
    end
  end

  for _, root in ipairs(roots) do
    print_tree(root, "")
  end

  return table.concat(lines, "\n")
end

-- Returns: multi-line string with full item details
-- Example:
-- id: 01KBBBXS6GKR2XS5T2QYHE5VGV
-- title: task title
-- created: 2025-11-30
-- completed: 2025-11-30T14:30:00
-- priority: 1
-- due: -1d → 2025-12-05 [+2d]
-- description: details here
-- blocks: 01KBC..., 01KBD...
-- log:
--   2025-11-30T10:00:00: message 1
--   2025-11-30T14:00:00: message 2
M.detail = function(item, opts)
  opts = opts or {}

  local lines = {}

  table.insert(lines, "id: " .. item.id:lower())
  table.insert(lines, "title: " .. item.title)
  table.insert(lines, "created: " .. (item.created or ""))

  if item.started then
    table.insert(lines, "started: " .. item.started)
  end

  if item.completed then
    table.insert(lines, "completed: " .. item.completed)
  end

  if item.priority then
    table.insert(lines, "priority: " .. item.priority)
  end

  -- Due date handling with resolution (use pre-computed value from enriched item)
  local resolved_due = item._computed and item._computed.resolved_due
  if item.due then
    if resolved_due then
      local relative = work.date_relative_to_today(resolved_due)
      if item.due == resolved_due then
        -- Absolute date that equals resolution
        table.insert(lines, "due: " .. resolved_due .. " [" .. relative .. "]")
      else
        -- Explicit relative or different resolution
        table.insert(lines, "due: " .. item.due .. " → " .. resolved_due .. " [" .. relative .. "]")
      end
    else
      -- Has explicit due but couldn't resolve
      table.insert(lines, "due: " .. item.due .. " (unresolved)")
    end
  elseif resolved_due then
    -- No explicit due, but we inferred one
    local relative = work.date_relative_to_today(resolved_due)
    table.insert(lines, "due: (inferred) → " .. resolved_due .. " [" .. relative .. "]")
  end

  if item.description then
    table.insert(lines, "description: " .. item.description)
  end

  if item.blocks and #item.blocks > 0 then
    local lowercase_blocks = {}
    for _, block_id in ipairs(item.blocks) do
      table.insert(lowercase_blocks, block_id:lower())
    end
    table.insert(lines, "blocks: " .. table.concat(lowercase_blocks, ", "))
  end

  if item.log then
    table.insert(lines, "log:")
    local timestamps = {}
    for ts in pairs(item.log) do
      table.insert(timestamps, ts)
    end
    table.sort(timestamps)
    for _, ts in ipairs(timestamps) do
      table.insert(lines, "  " .. ts .. ": " .. item.log[ts])
    end
  end

  return table.concat(lines, "\n")
end

-- Returns: JSON string
-- Input should already be cleaned (no _meta, no _computed)
M.json = function(data)
  local json = require("dkjson")
  return json.encode(data, { indent = true })
end

-- Generic function for rendering ready/blocked items
-- Options:
--   empty_message: message when no items
--   show_priority: show priority badge
--   show_blocks: show blocking dependencies
--   status_override: override status mark (e.g., "⊗" for blocked)
local function render_items_list(items, opts)
  opts = opts or {}

  if #items == 0 then
    return opts.empty_message or "no items"
  end

  local lines = {}
  for _, item in ipairs(items) do
    local status = opts.status_override or M.status_mark(item)

    local blockers = ""
    if opts.show_blocks and item._computed and item._computed.unresolved_blocks then
      local block_count = #item._computed.unresolved_blocks
      blockers = string.format("%2d↓ ", block_count)
    else
      blockers = "    "
    end

    local line = string.format("%s %s %s %s%s",
      status,
      M.short_id(item),
      M.due_display(item),
      blockers,
      item.title)

    if opts.show_priority then
      line = line .. M.priority_badge(item)
    end

    table.insert(lines, line)
  end

  return table.concat(lines, "\n")
end

-- Returns: multi-line string in ready format
-- Example:
-- ○ 2F2J +3w  title [p1]
-- ○ 4K5N      task with no due
--
-- Note: Uses ○ to indicate ready/unblocked status
M.ready = function(items, opts)
  return render_items_list(items, {
    empty_message = "no ready items",
    show_priority = true,
    show_blocks = false,
  })
end

-- Returns: multi-line string in blocked format
-- Example:
-- ⊗ 2F2J +3w  3↓ title
--
-- Note: Uses ⊗ to indicate blocked status, N↓ shows blocker count
-- Expects enriched items with _computed.unresolved_blocks
M.blocked = function(items, opts)
  return render_items_list(items, {
    empty_message = "no blocked items",
    show_priority = false,
    show_blocks = true,
    status_override = "⊗",
  })
end

return M
