-- work.nvim - completion for work files
local M = {}

local work = require("work")

-- Field names that can be completed
local FIELD_NAMES = {
  "blocks",
  "captured",
  "completed",
  "created",
  "description",
  "due",
  "id",
  "log",
  "priority",
  "title",
}

-- Get all work item IDs with metadata
local function get_all_ids_with_metadata()
  local items, err = work.load_items()
  if not items then
    return {}
  end

  local result = {}
  local enriched, enrich_err = work.get_all_enriched()
  if not enriched then
    return {}
  end

  for _, item in ipairs(enriched) do
    local short_id = work.short_id(item)
    local display = string.format("%s %s", short_id, item.title)
    if item._computed and item._computed.relative_due then
      display = display .. " [" .. item._computed.relative_due .. "]"
    end
    table.insert(result, {
      word = item.id,
      abbr = short_id,
      menu = item.title,
      info = display,
    })
  end

  return result
end

-- Get date suggestions
local function get_date_completions()
  local today = os.date("%Y-%m-%d")
  local function add_days(days)
    local time = os.time() + (days * 24 * 60 * 60)
    return os.date("%Y-%m-%d", time)
  end

  return {
    { word = today, menu = "today" },
    { word = add_days(1), menu = "tomorrow" },
    { word = add_days(7), menu = "+1 week" },
    { word = add_days(14), menu = "+2 weeks" },
    { word = add_days(30), menu = "+1 month" },
  }
end

-- Determine what to complete based on context
function M.complete(findstart, base)
  if findstart == 1 then
    local line = vim.api.nvim_get_current_line()
    local col = vim.api.nvim_win_get_cursor(0)[2]

    -- Find start of current word
    local start = col
    while start > 0 do
      local char = line:sub(start, start)
      if char:match("[%s=,{]") then
        break
      end
      start = start - 1
    end

    return start
  else
    local line = vim.api.nvim_get_current_line()
    local col = vim.api.nvim_win_get_cursor(0)[2]

    -- Determine context
    local before_cursor = line:sub(1, col)

    -- Complete field names at top level or after comma
    if before_cursor:match("^%s*$") or before_cursor:match(",%s*$") then
      local matches = {}
      for _, field in ipairs(FIELD_NAMES) do
        if field:find("^" .. vim.pesc(base), 1, false) then
          table.insert(matches, { word = field .. " = ", kind = "field" })
        end
      end
      return matches
    end

    -- Complete IDs in blocks array or after equals sign
    if before_cursor:match("blocks%s*=%s*{[^}]*$") or
       before_cursor:match('blocks%s*=%s*{[^}]*"%s*$') then
      local ids = get_all_ids_with_metadata()
      local matches = {}
      for _, id_item in ipairs(ids) do
        if base == "" or id_item.word:find(base, 1, true) or
           id_item.abbr:find(base, 1, true) or
           id_item.menu:lower():find(base:lower(), 1, true) then
          table.insert(matches, {
            word = '"' .. id_item.word .. '",',
            abbr = id_item.abbr,
            menu = id_item.menu,
            info = id_item.info,
          })
        end
      end
      return matches
    end

    -- Complete dates for due/created/completed fields
    if before_cursor:match("due%s*=%s*[\"']?$") or
       before_cursor:match("created%s*=%s*[\"']?$") or
       before_cursor:match("completed%s*=%s*[\"']?$") then
      local dates = get_date_completions()
      local matches = {}
      for _, date in ipairs(dates) do
        if base == "" or date.word:find(base, 1, true) then
          table.insert(matches, {
            word = '"' .. date.word .. '"',
            menu = date.menu,
          })
        end
      end
      return matches
    end

    -- Default: return field names
    local matches = {}
    for _, field in ipairs(FIELD_NAMES) do
      if field:find("^" .. vim.pesc(base), 1, false) then
        table.insert(matches, { word = field .. " = ", kind = "field" })
      end
    end
    return matches
  end
end

-- Setup omnifunc for work files
function M.setup()
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "lua",
    callback = function()
      local path = vim.fn.expand("%:p")
      if path:match("/progress/work/[^/]+%.lua$") then
        vim.bo.omnifunc = "v:lua.require'work.complete'.complete"
      end
    end,
  })
end

return M
