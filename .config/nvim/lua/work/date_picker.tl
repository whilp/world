-- luacheck ignore: neovim runtime
--check:false
local M = {}

local util = require("work.util")

local function format_date_item(date_str, days_offset)
  local year, month, day = date_str:match("(%d%d%d%d)-(%d%d)-(%d%d)")
  local date_time = os.time({ year = tonumber(year), month = tonumber(month), day = tonumber(day), hour = 0 })
  local day_name = os.date("%A", date_time)
  local month_name = os.date("%B", date_time)
  local day_num = tonumber(day)

  local relative
  if days_offset == 0 then
    relative = "0d (today)"
  else
    local sign = days_offset > 0 and "+" or ""
    local abs_days = math.abs(days_offset)
    if abs_days % 7 == 0 then
      local weeks = days_offset / 7
      relative = sign .. weeks .. "w (" .. sign .. days_offset .. "d)"
    else
      relative = sign .. days_offset .. "d"
    end
  end

  return string.format("%s - %s - %s, %s %d", relative, date_str, day_name, month_name, day_num)
end

function M.pick(callback, opts)
  opts = opts or {}
  local from = opts.from or 0
  local to = opts.to or 90

  local MiniPick = require("mini.pick")

  local items = {}
  local today = os.time()

  -- Add "Clear" option at top
  table.insert(items, {
    text = "Clear",
    date = nil,
  })

  for i = from, to do
    local date_time = today + (i * 86400)
    local date_str = os.date("%Y-%m-%d", date_time)
    table.insert(items, {
      text = format_date_item(date_str, i),
      date = date_str,
    })
  end

  local callback_called = false

  local source = {
    items = items,
    name = "Pick date",
    choose = function(chosen)
      callback_called = true
      if not chosen then
        callback(nil)
        return
      end
      callback(chosen.date)
    end,
  }

  -- Add custom mappings to handle Esc
  local mappings = {
    cancel = {
      char = "<Esc>",
      func = function()
        callback_called = true
        MiniPick.stop()
        callback(nil)
      end,
    },
  }

  MiniPick.start({ source = source, window = util.get_window_config(), mappings = mappings })
end

return M
