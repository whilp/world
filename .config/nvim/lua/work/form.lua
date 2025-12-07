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
      return "No blocks ('a' to add)"
    end
    local titles = {}
    for _, block_id in ipairs(block_ids) do
      local block_item, _ = work.get(block_id)
      if block_item then
        local label = work.short_id(block_item) .. ": " .. block_item.title
        if block_item.due then
          label = label .. " [" .. block_item.due .. "]"
        end
        table.insert(titles, label)
      end
    end
    return table.concat(titles, "\n")
  end

  local function add_blocks()
    local current_state = state:get_value()
    renderer:close()
    local picker = require("work.picker")
    picker.select_blocks(item.id, function(selected_ids)
      -- Merge with existing blocks, avoiding duplicates
      local block_set = {}
      for _, block_id in ipairs(current_state.blocks) do
        block_set[block_id] = true
      end
      for _, block_id in ipairs(selected_ids) do
        block_set[block_id] = true
      end

      local merged_blocks = {}
      for block_id, _ in pairs(block_set) do
        table.insert(merged_blocks, block_id)
      end

      local updated_item = vim.deepcopy(item)
      updated_item.title = current_state.title
      updated_item.due = current_state.due
      updated_item.description = current_state.description
      updated_item.blocks = merged_blocks
      M.edit(updated_item)
    end)
  end

  local function delete_blocks()
    local current_state = state:get_value()
    local current_blocks = current_state.blocks
    if #current_blocks == 0 then
      return
    end

    renderer:close()
    local MiniPick = require("mini.pick")

    local items = {}
    for _, block_id in ipairs(current_blocks) do
      local block_item, _ = work.get(block_id)
      if block_item then
        table.insert(items, {
          text = work.short_id(block_item) .. ": " .. block_item.title,
          id = block_id,
        })
      end
    end

    local source = {
      items = items,
      name = "Delete blocks",
      choose = function(chosen)
        if not chosen then
          M.edit(item)
          return
        end
        local new_blocks = {}
        for _, block_id in ipairs(current_blocks) do
          if block_id ~= chosen.id then
            table.insert(new_blocks, block_id)
          end
        end
        local updated_item = vim.deepcopy(item)
        updated_item.title = current_state.title
        updated_item.due = current_state.due
        updated_item.description = current_state.description
        updated_item.blocks = new_blocks
        M.edit(updated_item)
      end,
      choose_marked = function(marked)
        if not marked or #marked == 0 then
          M.edit(item)
          return
        end
        local delete_set = {}
        for _, chosen in ipairs(marked) do
          delete_set[chosen.id] = true
        end
        local new_blocks = {}
        for _, block_id in ipairs(current_blocks) do
          if not delete_set[block_id] then
            table.insert(new_blocks, block_id)
          end
        end
        local updated_item = vim.deepcopy(item)
        updated_item.title = current_state.title
        updated_item.due = current_state.due
        updated_item.description = current_state.description
        updated_item.blocks = new_blocks
        M.edit(updated_item)
      end,
    }

    local function get_window_config()
      return {
        config = function()
          local height = math.floor(vim.o.lines * 0.5)
          local width = math.floor(vim.o.columns * 0.6)
          return {
            anchor = "NW",
            height = height,
            width = width,
            row = math.floor((vim.o.lines - height) / 2),
            col = math.floor((vim.o.columns - width) / 2),
          }
        end
      }
    end

    MiniPick.start({ source = source, window = get_window_config() })
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
      n.paragraph({
        border_label = "Blocks ('a' to add, 'd' to delete)",
        lines = state.blocks:map(function(blocks) return format_blocks(blocks) end),
        is_focusable = true,
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
    {
      mode = { "n", "i" },
      key = "a",
      handler = add_blocks,
    },
    {
      mode = { "n", "i" },
      key = "d",
      handler = delete_blocks,
    },
  })

  renderer:render(body)
end

return M
