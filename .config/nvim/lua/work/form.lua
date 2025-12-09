local n = require("nui-components")
local work = require("work")
local util = require("work.util")

local M = {}

local validators = {
  title = function(v) return v ~= "" end,
  date = function(v) return v == "" or v:match("^%d%d%d%d%-%d%d%-%d%d$") end,
  due = function(v) return v == "" or v:match("^%d%d%d%d%-%d%d%-%d%d$") or v:match("^[+-]?%d+[dw]$") end,
  timestamp = function(v) return v == "" or v:match("^%d%d%d%d%-%d%d%-%d%dT%d%d:%d%d:%d%d$") end,
}


function M.edit(item_or_id)
  local item
  local is_new = false

  if item_or_id == nil then
    -- Creating a new item
    is_new = true
    item = {
      id = nil,  -- Will be assigned on submit
      title = "",
      due = nil,
      description = nil,
      started = nil,
      completed = nil,
      blocks = {},
      log = {},
      _is_new = true,  -- Mark as new for persistence across reopens
    }
  elseif type(item_or_id) == "string" then
    local loaded_item, err = work.get(item_or_id)
    if not loaded_item then
      vim.notify("work: " .. err, vim.log.levels.ERROR)
      return
    end
    item = loaded_item
  else
    item = item_or_id
    -- Preserve is_new flag across reopens
    if item._is_new then
      is_new = true
    end
  end

  -- Preserve the original log data across reopens
  local original_log = item._original_log or vim.deepcopy(item.log or {})
  local original = vim.deepcopy(item)

  local state = n.create_signal({
    title = item.title or "",
    due = item.due or "",
    description = item.description or "",
    started = item.started or "",
    completed = item.completed or "",
    blocks = item.blocks or {},
    log = item.log or {},
    logs_sort_asc = item._logs_sort_asc ~= nil and item._logs_sort_asc or false,  -- Default to descending (newest first)
    show_log_input = item._show_log_input or false,
    log_input_value = "",
    focused_section = item._focused_section or "title",
    autofocus_field = item._autofocus_field or "title",
    show_delete_confirm = item._show_delete_confirm or false,
  })


  local renderer = n.create_renderer({
    width = 80,
    height = 20,
    keymap = {
      focus_next = { "<Tab>", "<C-n>", "<Down>" },
      focus_prev = { "<S-Tab>", "<C-p>", "<Up>" },
    },
    window = {
      highlight = {
        FloatBorder = "FloatBorder",
        NormalFloat = "NormalFloat",
      },
    },
  })

  local function format_relative_days(days)
    if days == 0 then
      return "0d"
    end
    local sign = days > 0 and "+" or ""
    local abs_days = math.abs(days)
    if abs_days % 7 == 0 then
      return sign .. (days / 7) .. "w"
    end
    return sign .. days .. "d"
  end

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

  local function format_timestamp(ts, label)
    if not ts or ts == "" then
      return label
    end

    local year, month, day = ts:match("(%d%d%d%d)-(%d%d)-(%d%d)")
    if not year then
      return label
    end

    local ts_time = os.time({ year = tonumber(year), month = tonumber(month), day = tonumber(day), hour = 0 })
    local today_time = os.time({ year = os.date("%Y"), month = os.date("%m"), day = os.date("%d"), hour = 0 })
    local days_diff = math.floor((ts_time - today_time) / 86400)

    return format_relative_days(days_diff)
  end

  local function reopen_with_state(extra_fields)
    local current_state = state:get_value()
    local updated_item = vim.deepcopy(item)
    updated_item.title = current_state.title
    updated_item.due = current_state.due
    updated_item.description = current_state.description
    updated_item.started = current_state.started
    updated_item.completed = current_state.completed
    updated_item.blocks = current_state.blocks
    updated_item.log = current_state.log
    updated_item._focused_section = current_state.focused_section
    updated_item._autofocus_field = current_state.autofocus_field
    updated_item._logs_sort_asc = current_state.logs_sort_asc
    updated_item._original_log = original_log  -- Preserve original log across reopens
    updated_item._is_new = is_new  -- Preserve new item flag across reopens

    if extra_fields then
      for k, v in pairs(extra_fields) do
        updated_item[k] = v
      end
    end

    M.edit(updated_item)
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

  local function format_logs(log_table, sort_asc, original_log)
    local count = log_table and vim.tbl_count(log_table) or 0

    if not log_table or count == 0 then
      return "No logs (press Enter to add)"
    end

    local entries = {}
    for timestamp, message in pairs(log_table) do
      local is_draft = not (original_log and original_log[timestamp])
      table.insert(entries, { timestamp = timestamp, message = message, is_draft = is_draft })
    end

    -- Sort descending by default (newest first)
    table.sort(entries, function(a, b)
      if sort_asc then
        return a.timestamp < b.timestamp
      else
        return a.timestamp > b.timestamp
      end
    end)

    -- Limit to 5 most recent
    local lines = {}
    for i, entry in ipairs(entries) do
      if i > 5 then break end
      local time_part = entry.timestamp:match("T(.+)$") or entry.timestamp
      local bullet = entry.is_draft and "◉" or "●"
      local line = string.format("%s %s  %s", bullet, time_part, entry.message)
      table.insert(lines, line)
    end

    return table.concat(lines, "\n")
  end

  local function add_blocks()
    local current_blocks = state:get_value().blocks
    state.autofocus_field = "blocks"
    renderer:close()
    local picker = require("work.picker")
    picker.select_blocks(item.id, function(selected_ids)
      vim.schedule(function()
        if not selected_ids then
          reopen_with_state({ _autofocus_field = "blocks" })
          return
        end

        -- Merge with existing blocks, avoiding duplicates
        local block_set = {}
        for _, block_id in ipairs(current_blocks) do
          block_set[block_id] = true
        end
        for _, block_id in ipairs(selected_ids) do
          block_set[block_id] = true
        end

        local merged_blocks = {}
        for block_id, _ in pairs(block_set) do
          table.insert(merged_blocks, block_id)
        end

        reopen_with_state({ blocks = merged_blocks, _autofocus_field = "blocks" })
      end)
    end)
  end

  local function delete_blocks()
    local current_blocks = state:get_value().blocks
    if #current_blocks == 0 then
      return
    end

    state.autofocus_field = "blocks"
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

    local callback_called = false

    local source = {
      items = items,
      name = "Delete blocks",
      choose = function(chosen)
        callback_called = true
        vim.schedule(function()
          if not chosen then
            reopen_with_state({ _autofocus_field = "blocks" })
            return
          end

          local new_blocks = {}
          for _, block_id in ipairs(current_blocks) do
            if block_id ~= chosen.id then
              table.insert(new_blocks, block_id)
            end
          end

          reopen_with_state({ blocks = new_blocks, _autofocus_field = "blocks" })
        end)
      end,
      choose_marked = function(marked)
        callback_called = true
        vim.schedule(function()
          if not marked or #marked == 0 then
            reopen_with_state({ _autofocus_field = "blocks" })
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

          reopen_with_state({ blocks = new_blocks, _autofocus_field = "blocks" })
        end)
      end,
    }

    -- Add custom mappings to handle Esc
    local mappings = {
      cancel = {
        char = "<Esc>",
        func = function()
          callback_called = true
          MiniPick.stop()
          vim.schedule(function()
            reopen_with_state({ _autofocus_field = "blocks" })
          end)
        end,
      },
    }

    MiniPick.start({ source = source, window = util.get_window_config(), mappings = mappings })
  end

  local function add_log_entry()
    state.autofocus_field = "log_input"
    renderer:close()
    vim.schedule(function()
      reopen_with_state({ _show_log_input = true, _autofocus_field = "log_input" })
    end)
  end

  local function submit_log_entry()
    local message = state:get_value().log_input_value
    if message and message ~= "" then
      local timestamp = os.date("%Y-%m-%dT%H:%M:%S")
      local updated_log = vim.deepcopy(state:get_value().log)
      updated_log[timestamp] = message

      renderer:close()
      vim.schedule(function()
        reopen_with_state({ log = updated_log, _show_log_input = false, _autofocus_field = "logs" })
      end)
    end
  end

  local function cancel_log_entry()
    renderer:close()
    vim.schedule(function()
      reopen_with_state({ _show_log_input = false, _autofocus_field = "logs" })
    end)
  end

  local function toggle_logs_sort()
    local new_sort = not state:get_value().logs_sort_asc
    renderer:close()
    vim.schedule(function()
      reopen_with_state({ _logs_sort_asc = new_sort, _autofocus_field = "logs" })
    end)
  end

  local function show_delete_confirm()
    if is_new then
      vim.notify("work: cannot delete unsaved item", vim.log.levels.WARN)
      return
    end
    renderer:close()
    vim.schedule(function()
      reopen_with_state({ _show_delete_confirm = true })
    end)
  end

  local function confirm_delete()
    -- Get file path before deletion (item will be removed from store)
    local file_path = work.get_file_path(item.id)

    local deleted, del_err = work.delete(item.id)
    if not deleted then
      vim.notify("work: " .. del_err, vim.log.levels.ERROR)
      return
    end
    vim.notify("deleted: " .. work.short_id(item))

    -- Git rm and commit
    local git = require("work.git")
    git.commit(item.id, "delete", file_path)

    renderer:close()
  end

  local function cancel_delete()
    renderer:close()
    vim.schedule(function()
      reopen_with_state({ _show_delete_confirm = false })
    end)
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

  local function logs_changed(new_log, old_log)
    local new_count = vim.tbl_count(new_log)
    local old_count = vim.tbl_count(old_log)
    if new_count ~= old_count then return true end
    for timestamp, message in pairs(new_log) do
      if old_log[timestamp] ~= message then return true end
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

    if not validators.timestamp(state:get_value().started) then
      vim.notify("work: invalid started timestamp format", vim.log.levels.ERROR)
      return
    end

    if not validators.timestamp(state:get_value().completed) then
      vim.notify("work: invalid completed timestamp format", vim.log.levels.ERROR)
      return
    end

    local parsed_due = state:get_value().due
    if parsed_due ~= "" then
      local resolved = util.parse_relative_date(parsed_due)
      if resolved then
        parsed_due = resolved
      elseif not validators.date(parsed_due) then
        vim.notify("work: invalid due date format", vim.log.levels.ERROR)
        return
      end
    end

    -- Handle new item creation
    if is_new then
      local captured_timestamp = os.date("%Y-%m-%dT%H:%M:%S")
      local new_item, err = work.add(state:get_value().title, { captured = captured_timestamp })
      if not new_item then
        vim.notify("work: failed to create item: " .. err, vim.log.levels.ERROR)
        return
      end
      item.id = new_item.id
      vim.notify("created " .. work.short_id(new_item), vim.log.levels.INFO)

      -- Now update all other fields if they're set
      local updates = {}
      if parsed_due ~= "" then
        updates.due = parsed_due
      end
      if state:get_value().description ~= "" then
        updates.description = state:get_value().description
      end
      if state:get_value().started ~= "" then
        updates.started = state:get_value().started
      end
      if state:get_value().completed ~= "" then
        updates.completed = state:get_value().completed
      end

      if next(updates) then
        local result, update_err = work.update(item.id, updates)
        if not result or update_err then
          vim.notify("work: failed to update fields: " .. (update_err or "unknown error"), vim.log.levels.ERROR)
          return
        end
      end

      -- Set blocks if any
      if #state:get_value().blocks > 0 then
        local result, block_err = work.set_blocks(item.id, state:get_value().blocks)
        if not result or block_err then
          vim.notify("work: failed to set blocks: " .. (block_err or "unknown error"), vim.log.levels.ERROR)
          return
        end
      end

      -- Set logs if any
      if vim.tbl_count(state:get_value().log) > 0 then
        local result, log_err = work.update(item.id, { log = state:get_value().log })
        if not result or log_err then
          vim.notify("work: failed to set logs: " .. (log_err or "unknown error"), vim.log.levels.ERROR)
          return
        end
      end

      -- Commit the new item to git
      local git = require("work.git")
      git.commit(item.id, "add")

      renderer:close()
      return
    end

    -- Handle existing item updates
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

    if state:get_value().started ~= (original.started or "") then
      updates.started = state:get_value().started == "" and nil or state:get_value().started
      has_updates = true
    end

    if state:get_value().completed ~= (original.completed or "") then
      updates.completed = state:get_value().completed == "" and nil or state:get_value().completed
      has_updates = true
    end

    local blocks_updated = blocks_changed(state:get_value().blocks, original.blocks or {})
    local logs_updated = logs_changed(state:get_value().log, original.log or {})

    local any_changes = false

    if has_updates then
      local _, err = work.update(item.id, updates)
      if not err then
        vim.notify("updated " .. work.short_id(item), vim.log.levels.INFO)
        any_changes = true
      else
        vim.notify("work: failed to update: " .. err, vim.log.levels.ERROR)
        return
      end
    end

    if blocks_updated then
      local _, err = work.set_blocks(item.id, state:get_value().blocks)
      if err then
        vim.notify("work: failed to update blocks: " .. err, vim.log.levels.ERROR)
        return
      end
      vim.notify("updated blocks for " .. work.short_id(item), vim.log.levels.INFO)
      any_changes = true
    end

    if logs_updated then
      local _, err = work.update(item.id, { log = state:get_value().log })
      if err then
        vim.notify("work: failed to update logs: " .. err, vim.log.levels.ERROR)
        return
      end
      vim.notify("updated logs for " .. work.short_id(item), vim.log.levels.INFO)
      any_changes = true
    end

    -- Single git commit for all changes
    if any_changes then
      local git = require("work.git")
      git.commit(item.id, "update")
    end

    renderer:close()

    if not any_changes then
      vim.notify("no changes made", vim.log.levels.INFO)
    end
  end

  local body = function()
    local form_components = {
      n.text_input({
        autofocus = not state:get_value().show_log_input and not state:get_value().show_delete_confirm and state:get_value().autofocus_field == "title",
        border_label = "[T]itle *",
        max_lines = 1,
        value = state.title,
        on_change = function(value) state.title = value end,
        on_focus = function()
          state.autofocus_field = "title"
        end,
        window = {
          highlight = {
            NormalFloat = "Normal",
          },
        },
      }),
      n.button({
        border_label = "D[u]e",
        label = state.due:map(function(due) return format_due_date(due) end),
        autofocus = state:get_value().autofocus_field == "due",
        on_press = function()
          state.autofocus_field = "due"
          renderer:close()
          local date_picker = require("work.date_picker")
          date_picker.pick(function(selected_date)
            vim.schedule(function()
              reopen_with_state(selected_date and { due = selected_date, _autofocus_field = "due" })
            end)
          end)
        end,
        on_focus = function()
          state.autofocus_field = "due"
        end,
      }),
      n.text_input({
        border_label = "D[e]scription",
        max_lines = 10,
        autoresize = true,
        wrap = true,
        autofocus = state:get_value().autofocus_field == "description",
        value = state.description,
        on_change = function(value) state.description = value end,
        on_focus = function()
          state.autofocus_field = "description"
        end,
        window = {
          highlight = {
            NormalFloat = "Normal",
          },
        },
      }),
      n.button({
        border_label = "St[a]rted",
        label = state.started:map(function(ts) return format_timestamp(ts, "Not started") end),
        autofocus = state:get_value().autofocus_field == "started",
        on_press = function()
          state.autofocus_field = "started"
          renderer:close()
          local date_picker = require("work.date_picker")
          date_picker.pick(function(selected_date)
            vim.schedule(function()
              local ts = selected_date and (selected_date .. "T" .. os.date("%H:%M:%S"))
              reopen_with_state({ started = ts or "", _autofocus_field = "started" })
            end)
          end, { from = -30, to = 0 })
        end,
        on_focus = function()
          state.autofocus_field = "started"
        end,
      }),
      n.button({
        border_label = "C[o]mpleted",
        label = state.completed:map(function(ts) return format_timestamp(ts, "Not completed") end),
        autofocus = state:get_value().autofocus_field == "completed",
        on_press = function()
          state.autofocus_field = "completed"
          renderer:close()
          local date_picker = require("work.date_picker")
          date_picker.pick(function(selected_date)
            vim.schedule(function()
              local ts = selected_date and (selected_date .. "T" .. os.date("%H:%M:%S"))
              reopen_with_state({ completed = ts or "", _autofocus_field = "completed" })
            end)
          end, { from = -30, to = 0 })
        end,
        on_focus = function()
          state.autofocus_field = "completed"
        end,
      }),
      n.paragraph({
        border_label = "[B]locks (Enter to add, 'd' to delete)",
        lines = state.blocks:map(function(blocks) return format_blocks(blocks) end),
        is_focusable = true,
        autofocus = state:get_value().autofocus_field == "blocks",
        on_focus = function()
          state.focused_section = "blocks"
          state.autofocus_field = "blocks"
        end,
      }),
    }

    if state:get_value().show_log_input then
      table.insert(form_components, n.text_input({
        autofocus = true,
        border_label = "New log (Ctrl+Enter to submit, Esc to cancel)",
        max_lines = 3,
        value = state.log_input_value,
        on_change = function(value) state.log_input_value = value end,
        on_focus = function()
          state.focused_section = "log_input"
          state.autofocus_field = "log_input"
        end,
        window = {
          highlight = {
            NormalFloat = "Normal",
          },
        },
      }))
    end

    table.insert(form_components, n.paragraph({
      border_label = "[L]ogs (Enter to add, 's' to toggle sort)",
      lines = state.log:map(function(log)
        return format_logs(log, state.logs_sort_asc:get_value(), original_log)
      end),
      is_focusable = true,
      autofocus = state:get_value().autofocus_field == "logs",
      on_focus = function()
        state.focused_section = "logs"
        state.autofocus_field = "logs"
      end,
    }))

    -- Action buttons at the bottom
    local action_buttons = {}

    table.insert(action_buttons, n.button({
      label = "[S]ubmit",
      flex = 1,
      on_press = function()
        if state:get_value().show_log_input then
          submit_log_entry()
        else
          submit_form()
        end
      end,
    }))

    table.insert(action_buttons, n.button({
      label = "[C]ancel",
      flex = 1,
      on_press = function()
        renderer:close()
      end,
    }))

    if not is_new then
      table.insert(action_buttons, n.button({
        label = state.show_delete_confirm:map(function(confirm)
          return confirm and "Confirm? [Enter]" or "[D]elete"
        end),
        flex = 1,
        autofocus = state:get_value().show_delete_confirm,
        on_press = function()
          if state:get_value().show_delete_confirm then
            confirm_delete()
          else
            show_delete_confirm()
          end
        end,
        on_focus = function()
          if state:get_value().show_delete_confirm then
            state.focused_section = "delete_confirm"
          end
        end,
      }))
    end

    table.insert(form_components, n.columns(unpack(action_buttons)))

    return n.rows(unpack(form_components))
  end

  renderer:add_mappings({
    {
      mode = { "n" },
      key = "<C-s>",
      handler = function()
        if state:get_value().show_log_input then
          submit_log_entry()
        else
          submit_form()
        end
      end,
    },
    {
      mode = { "n" },
      key = "X",
      handler = show_delete_confirm,
    },
    {
      mode = { "n" },
      key = "y",
      handler = function()
        if state:get_value().show_delete_confirm then
          confirm_delete()
        end
      end,
    },
    {
      mode = { "n", "i" },
      key = "<C-CR>",
      handler = function()
        if state:get_value().show_log_input then
          submit_log_entry()
        else
          submit_form()
        end
      end,
    },
    {
      mode = { "n", "i" },
      key = "<D-CR>",
      handler = function()
        if state:get_value().show_log_input then
          submit_log_entry()
        else
          submit_form()
        end
      end,
    },
    {
      mode = { "n" },
      key = "<Esc>",
      handler = function()
        if state:get_value().show_delete_confirm then
          cancel_delete()
        elseif state:get_value().show_log_input then
          cancel_log_entry()
        else
          renderer:close()
        end
      end,
    },
    {
      mode = { "n" },
      key = "d",
      handler = function()
        if state:get_value().focused_section == "blocks" then
          delete_blocks()
        else
          show_delete_confirm()
        end
      end,
    },
    {
      mode = { "n" },
      key = "<CR>",
      handler = function()
        if state:get_value().show_delete_confirm then
          confirm_delete()
        elseif state:get_value().focused_section == "blocks" then
          add_blocks()
        elseif state:get_value().show_log_input then
          submit_log_entry()
        else
          add_log_entry()
        end
      end,
    },
    {
      mode = { "n" },
      key = "s",
      handler = function()
        if state:get_value().focused_section == "logs" then
          toggle_logs_sort()
        else
          submit_form()
        end
      end,
    },
    {
      mode = { "n" },
      key = "c",
      handler = function()
        if state:get_value().show_delete_confirm then
          cancel_delete()
        elseif state:get_value().show_log_input then
          cancel_log_entry()
        else
          renderer:close()
        end
      end,
    },
    {
      mode = { "n" },
      key = "t",
      handler = function()
        state.autofocus_field = "title"
        renderer:close()
        vim.schedule(function()
          reopen_with_state({ _autofocus_field = "title" })
        end)
      end,
    },
    {
      mode = { "n" },
      key = "u",
      handler = function()
        state.autofocus_field = "due"
        renderer:close()
        vim.schedule(function()
          reopen_with_state({ _autofocus_field = "due" })
        end)
      end,
    },
    {
      mode = { "n" },
      key = "e",
      handler = function()
        state.autofocus_field = "description"
        renderer:close()
        vim.schedule(function()
          reopen_with_state({ _autofocus_field = "description" })
        end)
      end,
    },
    {
      mode = { "n" },
      key = "b",
      handler = function()
        state.autofocus_field = "blocks"
        renderer:close()
        vim.schedule(function()
          reopen_with_state({ _autofocus_field = "blocks" })
        end)
      end,
    },
    {
      mode = { "n" },
      key = "l",
      handler = function()
        state.autofocus_field = "logs"
        renderer:close()
        vim.schedule(function()
          reopen_with_state({ _autofocus_field = "logs" })
        end)
      end,
    },
    {
      mode = { "n" },
      key = "a",
      handler = function()
        state.autofocus_field = "started"
        renderer:close()
        vim.schedule(function()
          reopen_with_state({ _autofocus_field = "started" })
        end)
      end,
    },
    {
      mode = { "n" },
      key = "o",
      handler = function()
        state.autofocus_field = "completed"
        renderer:close()
        vim.schedule(function()
          reopen_with_state({ _autofocus_field = "completed" })
        end)
      end,
    },
  })

  renderer:render(body)
end

return M
