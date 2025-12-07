local n = require("nui-components")
local work = require("work")

local M = {}

local validators = {
  title = function(v) return v ~= "" end,
  date = function(v) return v == "" or v:match("^%d%d%d%d%-%d%d%-%d%d$") end,
  due = function(v) return v == "" or v:match("^%d%d%d%d%-%d%d%-%d%d$") or v:match("^[+-]?%d+[dw]$") end,
}

local function parse_relative_date(input)
  if not input or input == "" then
    return nil
  end

  input = input:lower():gsub("^%s*(.-)%s*$", "%1")

  if input == "today" or input == "0d" then
    return os.date("%Y-%m-%d")
  end

  if input == "tomorrow" or input == "+1d" then
    return os.date("%Y-%m-%d", os.time() + 86400)
  end

  local sign, num, unit = input:match("^([+-]?)(%d+)([dw])$")
  if num and unit then
    num = tonumber(num)
    local seconds_map = {
      d = 86400,
      w = 86400 * 7,
    }
    local seconds = seconds_map[unit]
    if seconds then
      if sign == "-" then
        return os.date("%Y-%m-%d", os.time() - (num * seconds))
      else
        return os.date("%Y-%m-%d", os.time() + (num * seconds))
      end
    end
  end

  if input:match("^%d%d%d%d%-%d%d%-%d%d$") then
    return input
  end

  return nil
end

function M.edit(item_or_id)
  local item
  if type(item_or_id) == "string" then
    local loaded_item, err = work.get(item_or_id)
    if not loaded_item then
      vim.notify("work: " .. err, vim.log.levels.ERROR)
      return
    end
    item = loaded_item
  else
    item = item_or_id
  end

  local original = vim.deepcopy(item)

  local state = n.create_signal({
    title = item.title or "",
    due = item.due or "",
    description = item.description or "",
  })

  local renderer = n.create_renderer({
    width = 80,
    height = 20,
  })

  local function submit_form()
    if not validators.title(state:get_value().title) then
      vim.notify("work: title is required", vim.log.levels.ERROR)
      return
    end

    if not validators.due(state:get_value().due) then
      vim.notify("work: invalid due date format", vim.log.levels.ERROR)
      return
    end

    local parsed_due = state:get_value().due
    if parsed_due ~= "" then
      local resolved = parse_relative_date(parsed_due)
      if resolved then
        parsed_due = resolved
      elseif not validators.date(parsed_due) then
        vim.notify("work: invalid due date format", vim.log.levels.ERROR)
        return
      end
    end

    local updates = {}
    local has_updates = false

    if state:get_value().title ~= original.title then
      updates.title = state:get_value().title
      has_updates = true
    end

    if parsed_due ~= (original.due or "") then
      updates.due = parsed_due == "" and nil or parsed_due
      has_updates = true
    end

    if state:get_value().description ~= (original.description or "") then
      updates.description = state:get_value().description == "" and nil or state:get_value().description
      has_updates = true
    end

    if has_updates then
      local _, err = work.update(item.id, updates)
      if not err then
        vim.notify("updated " .. work.short_id(item), vim.log.levels.INFO)
      else
        vim.notify("work: failed to update: " .. err, vim.log.levels.ERROR)
        return
      end
      local git = require("work.git")
      git.commit(item.id, "update")
    end

    renderer:close()

    if not has_updates then
      vim.notify("no changes made", vim.log.levels.INFO)
    end
  end

  local body = function()
    return n.rows(
      { flex = 0 },
      n.text_input({
        autofocus = true,
        border_label = "Title *",
        max_lines = 1,
        value = state.title,
        on_change = function(value) state.title = value end,
      }),
      n.text_input({
        border_label = "Due (YYYY-MM-DD or +1d, +2w)",
        max_lines = 1,
        value = state.due,
        on_change = function(value) state.due = value end,
      }),
      n.text_input({
        border_label = "Description",
        max_lines = 10,
        autoresize = true,
        wrap = true,
        value = state.description,
        on_change = function(value) state.description = value end,
      })
    )
  end

  renderer:add_mappings({
    {
      mode = { "n", "i" },
      key = "<C-CR>",
      handler = submit_form,
    },
    {
      mode = { "n", "i" },
      key = "<D-CR>",
      handler = submit_form,
    },
    {
      mode = { "n" },
      key = "<Esc>",
      handler = function()
        renderer:close()
      end,
    },
  })

  renderer:render(body)
end

return M
