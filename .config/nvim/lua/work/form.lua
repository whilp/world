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
    blocks = item.blocks or {},
  })

  local renderer = n.create_renderer({
    width = 80,
    height = 20,
  })

  local function format_due_date(due_date)
    if not due_date or due_date == "" then
      return "Set due date"
    end

    local year, month, day = due_date:match("(%d+)-(%d+)-(%d+)")
    if not year then
      return "Set due date"
    end

    local due_time = os.time({ year = tonumber(year), month = tonumber(month), day = tonumber(day), hour = 0 })
    local today_time = os.time({ year = os.date("%Y"), month = os.date("%m"), day = os.date("%d"), hour = 0 })
    local days_diff = math.floor((due_time - today_time) / 86400)

    local relative
    if days_diff == 0 then
      relative = "today"
    elseif days_diff == 1 then
      relative = "tomorrow"
    elseif days_diff == -1 then
      relative = "yesterday"
    elseif days_diff < 0 then
      relative = math.abs(days_diff) .. "d ago"
    else
      relative = days_diff .. "d"
    end

    return string.format("%s (%s)", due_date, relative)
  end

  local function format_blocks(block_ids)
    if not block_ids or #block_ids == 0 then
      return "No blocks"
    end
    local titles = {}
    for _, block_id in ipairs(block_ids) do
      local block_item, _ = work.get(block_id)
      if block_item then
        table.insert(titles, work.short_id(block_item) .. ": " .. block_item.title)
      end
    end
    return table.concat(titles, "\n")
  end

  local function blocks_changed(new_blocks, old_blocks)
    if #new_blocks ~= #old_blocks then return true end
    local old_set = {}
    for _, id in ipairs(old_blocks) do
      old_set[id] = true
    end
    for _, id in ipairs(new_blocks) do
      if not old_set[id] then return true end
    end
    return false
  end

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

    local blocks_updated = blocks_changed(state:get_value().blocks, original.blocks or {})

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

    if blocks_updated then
      local _, err = work.set_blocks(item.id, state:get_value().blocks)
      if err then
        vim.notify("work: failed to update blocks: " .. err, vim.log.levels.ERROR)
        return
      end
      vim.notify("updated blocks for " .. work.short_id(item), vim.log.levels.INFO)
      local git = require("work.git")
      git.commit(item.id, "update")
    end

    renderer:close()

    if not has_updates and not blocks_updated then
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
      n.button({
        border_label = "Due",
        label = state.due:map(function(due) return format_due_date(due) end),
        on_press = function()
          local current_state = state:get_value()
          renderer:close()
          local date_picker = require("work.date_picker")
          date_picker.pick(function(selected_date)
            local updated_item = vim.deepcopy(item)
            updated_item.title = current_state.title
            updated_item.due = selected_date
            updated_item.description = current_state.description
            updated_item.blocks = current_state.blocks
            M.edit(updated_item)
          end)
        end,
      }),
      n.text_input({
        border_label = "Description",
        max_lines = 10,
        autoresize = true,
        wrap = true,
        value = state.description,
        on_change = function(value) state.description = value end,
      }),
      n.columns(
        { flex = 0 },
        n.paragraph({
          flex = 1,
          border_label = "Blocks",
          lines = state.blocks:map(function(blocks) return format_blocks(blocks) end),
          is_focusable = false,
        }),
        n.button({
          label = "Select blocks",
          on_press = function()
            local current_state = state:get_value()
            renderer:close()
            local picker = require("work.picker")
            picker.select_blocks(item.id, function(selected_ids)
              local updated_item = vim.deepcopy(item)
              updated_item.title = current_state.title
              updated_item.due = current_state.due
              updated_item.description = current_state.description
              updated_item.blocks = selected_ids
              M.edit(updated_item)
            end)
          end,
        })
      )
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
