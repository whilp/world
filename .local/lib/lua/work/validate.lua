-- Centralized validation for work system
-- Extracted from CLI and consolidated

local M = {}

-- Validate field update
-- Returns: ok, err
M.field_update = function(item, key, value)
  if key == "id" or key == "created" then
    return nil, "cannot update field: " .. key
  end

  if key == "completed" then
    -- Validate timestamp format
    if value ~= "" and not value:match("^%d%d%d%d%-%d%d%-%d%dT%d%d:%d%d:%d%d$") then
      return nil, "completed must be YYYY-MM-DDTHH:MM:SS format"
    end
  end

  if key == "due" then
    local ok, err = M.due_date(value)
    if not ok then
      return nil, err
    end
  end

  if key == "priority" then
    local num = tonumber(value)
    if not num then
      return nil, "priority must be a number"
    end
  end

  return true
end

-- Validate due date format
-- Returns: ok, err
M.due_date = function(value)
  if not value or value == "" then
    return true
  end

  -- Validate date format (absolute or relative)
  local is_absolute = value:match("^%d%d%d%d%-%d%d%-%d%d$")
  local is_relative = value:match("^[+-]?%d+[dw]$")

  if not is_absolute and not is_relative then
    return nil, "due must be YYYY-MM-DD or relative format like -7d or +2w"
  end

  return true
end

-- Parse blocks argument string (comma-separated IDs)
-- resolve_fn should be a function that takes a short_id and returns full_id, err
-- Returns: blocks array, err
M.parse_blocks = function(blocks_str, resolve_fn)
  if not blocks_str or blocks_str == "" then
    return {}, nil
  end

  local blocks = {}
  for short_id in blocks_str:gmatch("[^,]+") do
    -- Trim whitespace
    short_id = short_id:match("^%s*(.-)%s*$")

    local full_id, err = resolve_fn(short_id)
    if not full_id then
      return nil, err
    end
    table.insert(blocks, full_id)
  end

  return blocks
end

return M
