local n = require("nui-components")
local work = require("work")

local M = {}

local validators = {
  title = function(v) return v ~= "" end,
  date = function(v) return v == "" or v:match("^%d%d%d%d%-%d%d%-%d%d$") end,
  datetime = function(v) return v == "" or v:match("^%d%d%d%d%-%d%d%-%d%dT%d%d:%d%d:%d%d$") end,
  due = function(v) return v == "" or v:match("^%d%d%d%d%-%d%d%-%d%d$") or v:match("^[+-]?%d+[dw]$") end,
  priority = function(v) return v == "" or (tonumber(v) and tonumber(v) >= 0) end,
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
    priority = tostring(item.priority or ""),
    due = item.due or "",
    description = item.description or "",
    created = item.created or "",
    started = item.started or "",
    completed = item.completed or "",
    blocks = item.blocks or {},
    new_log = "",
    active_tab = "main",
  })

  local is_tab_active = n.is_active_factory(state.active_tab)

  local renderer = n.create_renderer({
    width = 80,
    height = 40,
  })

  local function get_block_display()
    local blocks = state:get_value().blocks
    if not blocks or #blocks == 0 then
      return "no dependencies"
    end

    local lines = {}
    for _, block_id in ipairs(blocks) do
      local block_item, err = work.get(block_id)
      if block_item then
        local short_id = work.short_id(block_item)
        table.insert(lines, short_id .. ": " .. block_item.title)
      else
        table.insert(lines, block_id .. " (not found)")
      end
    end
    return table.concat(lines, "\n")
  end

  local function get_log_display()
    if not item.log or not next(item.log) then
      return "no log entries"
    end

    local sorted_timestamps = {}
    for timestamp, _ in pairs(item.log) do
      table.insert(sorted_timestamps, timestamp)
    end
    table.sort(sorted_timestamps, function(a, b) return a > b end)

    local lines = {}
    for _, timestamp in ipairs(sorted_timestamps) do
      table.insert(lines, "[" .. timestamp .. "] " .. item.log[timestamp])
    end
    return table.concat(lines, "\n")
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

    if not validators.priority(state:get_value().priority) then
      vim.notify("work: invalid priority value", vim.log.levels.ERROR)
      return
    end

    if not validators.date(state:get_value().created) then
      vim.notify("work: invalid created date format", vim.log.levels.ERROR)
      return
    end

    if not validators.datetime(state:get_value().started) then
      vim.notify("work: invalid started datetime format", vim.log.levels.ERROR)
      return
    end

    if not validators.datetime(state:get_value().completed) then
      vim.notify("work: invalid completed datetime format", vim.log.levels.ERROR)
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

    local new_priority = state:get_value().priority == "" and nil or tonumber(state:get_value().priority)
    local old_priority = original.priority
    if new_priority ~= old_priority then
      updates.priority = new_priority
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

    if state:get_value().created ~= (original.created or "") then
      updates.created = state:get_value().created == "" and nil or state:get_value().created
      has_updates = true
    end

    if state:get_value().started ~= (original.started or "") then
      updates.started = state:get_value().started == "" and nil or state:get_value().started
      has_updates = true
    end

    if state:get_value().completed ~= (original.completed or "") then
      updates.completed = state:get_value().completed == "" and nil or state:get_value().completed
      has_updates = true
    end

    local blocks_changed = false
    if #state:get_value().blocks ~= #(original.blocks or {}) then
      blocks_changed = true
    else
      for i, block_id in ipairs(state:get_value().blocks) do
        if block_id ~= (original.blocks[i] or "") then
          blocks_changed = true
          break
        end
      end
    end

    if blocks_changed then
      updates.blocks = #state:get_value().blocks == 0 and {} or state:get_value().blocks
      has_updates = true
    end

    local has_log = state:get_value().new_log ~= ""

    if has_log then
      local timestamp, err = work.add_log(item.id, state:get_value().new_log)
      if timestamp then
        vim.notify("added log entry", vim.log.levels.INFO)
      else
        vim.notify("work: failed to add log: " .. (err or "unknown error"), vim.log.levels.ERROR)
        return
      end
    end

    if has_updates then
      local _, err = work.update(item.id, updates)
      if not err then
        vim.notify("updated " .. work.short_id(item), vim.log.levels.INFO)
      else
        vim.notify("work: failed to update: " .. err, vim.log.levels.ERROR)
        return
      end
    end

    if has_updates or has_log then
      local git = require("work.git")
      git.commit(item.id, "update")
    end

    renderer:close()

    if not has_updates and not has_log then
      vim.notify("no changes made", vim.log.levels.INFO)
    end
  end

  local function handle_add_dependency()
    renderer:close()
    local picker = require("work.picker")
    picker.select_blocks(item.id, function(selected_ids)
      local existing_blocks = state:get_value().blocks
      local block_set = {}
      for _, block_id in ipairs(existing_blocks) do
        block_set[block_id] = true
      end
      for _, block_id in ipairs(selected_ids) do
        block_set[block_id] = true
      end

      local new_blocks = {}
      for block_id, _ in pairs(block_set) do
        table.insert(new_blocks, block_id)
      end

      state.blocks = new_blocks
      M.edit(item)
    end)
  end

  local function handle_clear_blocks()
    state.blocks = {}
  end

  local body = n.tabs(
    { active_tab = state.active_tab },
    n.columns(
      { flex = 0 },
      n.button({
        label = "1 Main",
        global_press_key = "1",
        is_active = is_tab_active("main"),
        on_press = function() state.active_tab = "main" end,
      }),
      n.gap(1),
      n.button({
        label = "2 Dates",
        global_press_key = "2",
        is_active = is_tab_active("dates"),
        on_press = function() state.active_tab = "dates" end,
      }),
      n.gap(1),
      n.button({
        label = "3 Deps",
        global_press_key = "3",
        is_active = is_tab_active("deps"),
        on_press = function() state.active_tab = "deps" end,
      }),
      n.gap(1),
      n.button({
        label = "4 Log",
        global_press_key = "4",
        is_active = is_tab_active("log"),
        on_press = function() state.active_tab = "log" end,
      })
    ),
    n.gap(1),
    n.tab(
      { id = "main" },
      n.rows(
        { flex = 0 },
        n.gap(1),
        n.text_input({
          autofocus = true,
          border_label = "Title *",
          max_lines = 1,
          value = state.title,
          on_change = function(value) state.title = value end,
        }),
        n.gap(1),
        n.columns(
          { flex = 0 },
          n.text_input({
            border_label = "Priority",
            max_lines = 1,
            value = state.priority,
            on_change = function(value) state.priority = value end,
          }),
          n.gap(2),
          n.text_input({
            border_label = "Due (YYYY-MM-DD or +1d, +2w)",
            max_lines = 1,
            value = state.due,
            on_change = function(value) state.due = value end,
          })
        ),
        n.gap(1),
        n.text_input({
          border_label = "Description",
          max_lines = 10,
          value = state.description,
          on_change = function(value) state.description = value end,
        })
      )
    ),
    n.tab(
      { id = "dates" },
      n.rows(
        { flex = 0 },
        n.gap(1),
        n.text_input({
          border_label = "Created (YYYY-MM-DD)",
          max_lines = 1,
          value = state.created,
          on_change = function(value) state.created = value end,
        }),
        n.gap(1),
        n.text_input({
          border_label = "Started (YYYY-MM-DDTHH:MM:SS)",
          max_lines = 1,
          value = state.started,
          on_change = function(value) state.started = value end,
        }),
        n.gap(1),
        n.text_input({
          border_label = "Completed (YYYY-MM-DDTHH:MM:SS)",
          max_lines = 1,
          value = state.completed,
          on_change = function(value) state.completed = value end,
        })
      )
    ),
    n.tab(
      { id = "deps" },
      n.rows(
        { flex = 0 },
        n.gap(1),
        n.paragraph({
          lines = vim.split(get_block_display(), "\n"),
          is_focusable = false,
        }),
        n.gap(1),
        n.columns(
          { flex = 0 },
          n.button({
            label = "Add dependency",
            padding = { left = 1, right = 1 },
            on_press = handle_add_dependency,
          }),
          n.gap(2),
          n.button({
            label = "Clear all",
            padding = { left = 1, right = 1 },
            on_press = handle_clear_blocks,
          })
        )
      )
    ),
    n.tab(
      { id = "log" },
      n.rows(
        { flex = 1 },
        n.gap(1),
        n.paragraph({
          lines = vim.split(get_log_display(), "\n"),
          is_focusable = false,
        }),
        n.gap(1),
        n.text_input({
          border_label = "New log entry",
          max_lines = 5,
          value = state.new_log,
          on_change = function(value) state.new_log = value end,
        })
      )
    )
  )

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
      key = "<Tab>",
      handler = function()
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Tab>", true, false, true), "n", false)
      end,
    },
    {
      mode = { "n", "i" },
      key = "<S-Tab>",
      handler = function()
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<S-Tab>", true, false, true), "n", false)
      end,
    },
  })

  renderer:render(body)
end

return M
