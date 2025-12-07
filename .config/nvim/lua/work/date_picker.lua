local M = {}

local util = require("work.util")

local function format_date_item(date_str, days_offset)
  local date_obj = os.time({ year = tonumber(date_str:sub(1, 4)), month = tonumber(date_str:sub(6, 7)), day = tonumber(date_str:sub(9, 10)) })
  local day_name = os.date("%A", date_obj)
  local month_name = os.date("%B", date_obj)
  local day_num = os.date("%d", date_obj)

  local relative
  if days_offset == 0 then
    relative = "today"
  elseif days_offset == 1 then
    relative = "tomorrow"
  elseif days_offset < 7 then
    relative = "in " .. days_offset .. " days"
  elseif days_offset < 14 then
    relative = "next week"
  elseif days_offset < 30 then
    relative = "in " .. math.floor(days_offset / 7) .. " weeks"
  elseif days_offset < 60 then
    relative = "next month"
  else
    relative = "in " .. math.floor(days_offset / 30) .. " months"
  end

  return string.format("%s - %s, %s %s (%s)", date_str, day_name, month_name, day_num, relative)
end

function M.pick(callback)
  local MiniPick = require("mini.pick")

  local items = {}
  local today = os.time()

  for i = 0, 90 do
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
