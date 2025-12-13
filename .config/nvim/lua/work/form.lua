-- work.form - native implementation without nui-components
local work = require("work")
local util = require("work.util")

local M = {}

-- Module state (per-form instance)
local buf = nil
local win = nil
local state = nil
local ns = vim.api.nvim_create_namespace("work_form")
local autocmd_group = nil

-- Field definitions
local FIELDS = {
  { name = "title", label = "Title *", type = "text_input", multiline = false },
  { name = "due", label = "Due", type = "button" },
  { name = "description", label = "Description", type = "text_input", multiline = true },
  { name = "started", label = "Started", type = "button" },
  { name = "completed", label = "Completed", type = "button" },
  { name = "blocks", label = "Blocks", type = "paragraph" },
  { name = "logs", label = "Logs", type = "paragraph" },
}

-- Validators
local validators = {
  title = function(v) return v ~= "" end,
  date = function(v) return v == "" or v:match("^%d%d%d%d%-%d%d%-%d%d$") end,
  due = function(v) return v == "" or v:match("^%d%d%d%d%-%d%d%-%d%d$") or v:match("^[+-]?%d+[dw]$") end,
  timestamp = function(v) return v == "" or v:match("^%d%d%d%d%-%d%d%-%d%dT%d%d:%d%d:%d%d$") end,
}

-- Helper functions for date/timestamp formatting
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

local function format_blocks(block_ids)
  if not block_ids or #block_ids == 0 then
    return { "No blocks ('a' to add)" }
  end
  local lines = {}
  for _, block_id in ipairs(block_ids) do
    local block_item, _ = work.get(block_id)
    if block_item then
      local label = work.short_id(block_item) .. ": " .. block_item.title
      if block_item.due then
        label = label .. " [" .. block_item.due .. "]"
      end
      table.insert(lines, label)
    end
  end
  return lines
end

local function format_logs(log_table, sort_asc, original_log)
  local count = log_table and vim.tbl_count(log_table) or 0

  if not log_table or count == 0 then
    return { "No logs (press Enter to add)" }
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

  return lines
end

-- Rendering helpers
local function pad_line(text, width)
  local len = vim.fn.strdisplaywidth(text)
  if len >= width then
    return text:sub(1, width)
  end
  return text .. string.rep(" ", width - len)
end

local function get_field_at_line(line_num)
  if not state or not state.field_positions then return nil end
  for name, pos in pairs(state.field_positions) do
    -- pos.start is label line, pos.end is last content line (1-indexed)
    if line_num >= pos.start and line_num <= pos["end"] then
      local field_def = nil
      for _, f in ipairs(FIELDS) do
        if f.name == name then
          field_def = f
          break
        end
      end
      -- Check if it's the log_input field
      if not field_def and name == "log_input" then
        field_def = { name = "log_input", type = "text_input", multiline = true }
      end
      return field_def, pos
    end
  end
  return nil
end

local function get_field_def(name)
  for _, f in ipairs(FIELDS) do
    if f.name == name then
      return f
    end
  end
  if name == "log_input" then
    return { name = "log_input", type = "text_input", multiline = true }
  end
  return nil
end

-- Content extraction from buffer
local function extract_field_content(field_name)
  local pos = state.field_positions[field_name]
  if not pos then return "" end

  -- pos.start is label line, pos.start+1 is first content line (1-indexed)
  -- Convert to 0-indexed for nvim_buf_get_lines
  local content_start = pos.start  -- Skip label line (0-indexed: pos.start)
  local content_end = pos["end"]    -- Exclusive end for get_lines

  if content_start >= content_end then return "" end

  local lines = vim.api.nvim_buf_get_lines(buf, content_start, content_end, false)

  local field_def = get_field_def(field_name)
  if field_def and not field_def.multiline then
    return lines[1] or ""
  else
    return table.concat(lines, "\n")
  end
end

-- Sync state from buffer (for text_input fields)
local function sync_state_from_buffer()
  if not state or not buf or not vim.api.nvim_buf_is_valid(buf) then return end

  for name, pos in pairs(state.field_positions) do
    local field_def = get_field_def(name)
    if field_def and field_def.type == "text_input" then
      local content = extract_field_content(name)
      if name == "log_input" then
        state.ui.log_input_value = content
      else
        state.data[name] = content
      end
    end
  end
end

-- Highlight application (must be defined before render())
local function apply_highlights()
  if not buf or not vim.api.nvim_buf_is_valid(buf) then return end
  if not state or not state.field_positions then return end

  vim.api.nvim_buf_clear_namespace(buf, ns, 0, -1)

  for name, pos in pairs(state.field_positions) do
    local is_focused = (name == state.ui.focused_field)
    local hl_label = is_focused and "Title" or "Comment"

    -- Highlight the label line (pos.start is 1-indexed, convert to 0-indexed)
    vim.api.nvim_buf_add_highlight(buf, ns, hl_label, pos.start - 1, 0, -1)
  end
end

-- Focus management (must be defined before render())
local function get_visible_fields()
  local visible = {}
  for _, field in ipairs(FIELDS) do
    table.insert(visible, field)
  end
  if state and state.ui.show_log_input then
    -- Insert before logs
    local logs_idx = nil
    for i, f in ipairs(visible) do
      if f.name == "logs" then
        logs_idx = i
        break
      end
    end
    if logs_idx then
      table.insert(visible, logs_idx, { name = "log_input" })
    end
  end
  return visible
end

local function focus_field(field_name, skip_render)
  if not state or not buf or not vim.api.nvim_buf_is_valid(buf) then return end

  local pos = state.field_positions[field_name]
  if not pos then
    -- Field not visible, use default
    field_name = "title"
    pos = state.field_positions[field_name]
  end

  if not pos then return end

  -- Update state
  state.ui.focused_field = field_name

  -- Update highlights if not skipping render
  if not skip_render then
    apply_highlights()
  end

  -- Move cursor to first content line (pos.start+1, since pos.start is label)
  if win and vim.api.nvim_win_is_valid(win) then
    vim.api.nvim_win_set_cursor(win, { pos.start + 1, 0 })
  end
end

local function focus_next()
  local visible = get_visible_fields()
  local current_idx = nil
  for i, f in ipairs(visible) do
    if f.name == state.ui.focused_field then
      current_idx = i
      break
    end
  end

  if not current_idx then current_idx = 0 end
  local next_idx = (current_idx % #visible) + 1
  focus_field(visible[next_idx].name)
end

local function focus_prev()
  local visible = get_visible_fields()
  local current_idx = nil
  for i, f in ipairs(visible) do
    if f.name == state.ui.focused_field then
      current_idx = i
      break
    end
  end

  if not current_idx then current_idx = 2 end
  local prev_idx = ((current_idx - 2) % #visible) + 1
  focus_field(visible[prev_idx].name)
end

-- Rendering function
local function render()
  if not buf or not vim.api.nvim_buf_is_valid(buf) then return end

  local lines = {}
  local field_positions = {}

  -- Build visible fields list
  local visible_fields = {}
  for _, field in ipairs(FIELDS) do
    table.insert(visible_fields, field)
  end

  -- Insert log_input field conditionally
  if state.ui.show_log_input then
    -- Insert before logs field
    local logs_idx = nil
    for i, f in ipairs(visible_fields) do
      if f.name == "logs" then
        logs_idx = i
        break
      end
    end
    if logs_idx then
      table.insert(visible_fields, logs_idx, {
        name = "log_input",
        label = "New log (Ctrl+Enter to submit, Esc to cancel)",
        type = "text_input",
        multiline = true
      })
    end
  end

  -- Render each field
  for _, field in ipairs(visible_fields) do
    local start_line = #lines + 1  -- 1-indexed

    -- Field label
    table.insert(lines, field.label)

    -- Content lines
    local content_lines
    if field.type == "text_input" then
      local value = field.name == "log_input" and state.ui.log_input_value or (state.data[field.name] or "")
      if field.multiline then
        local value_lines = vim.split(value, "\n", { plain = true })
        content_lines = value_lines
        if #content_lines == 0 then
          content_lines = { "" }
        end
      else
        content_lines = { value }
      end
    elseif field.type == "button" then
      local value
      if field.name == "due" then
        value = format_due_date(state.data.due)
      elseif field.name == "started" then
        value = format_timestamp(state.data.started, "Not started")
      elseif field.name == "completed" then
        value = format_timestamp(state.data.completed, "Not completed")
      else
        value = state.data[field.name] or ""
      end
      content_lines = { value }
    elseif field.type == "paragraph" then
      if field.name == "blocks" then
        content_lines = format_blocks(state.data.blocks)
      elseif field.name == "logs" then
        content_lines = format_logs(state.data.log, state.ui.logs_sort_asc, state.original_log)
      else
        content_lines = { "" }
      end
    end

    for _, line in ipairs(content_lines) do
      table.insert(lines, line)
    end

    -- Blank line separator
    table.insert(lines, "")

    -- Store field positions: start = label line, end = last content line (1-indexed)
    field_positions[field.name] = {
      start = start_line,
      ["end"] = #lines - 1  -- Exclude the blank separator
    }
  end

  -- Action buttons row
  local buttons_line
  if state.ui.show_delete_confirm then
    buttons_line = "[S]ubmit  [C]ancel  Confirm? [Enter]"
  elseif state.is_new then
    buttons_line = "[S]ubmit  [C]ancel"
  else
    buttons_line = "[S]ubmit  [C]ancel  [D]elete"
  end
  table.insert(lines, buttons_line)

  -- Update buffer
  vim.api.nvim_buf_set_option(buf, "modifiable", true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)

  -- Store field positions
  state.field_positions = field_positions

  -- Apply highlights
  apply_highlights()

  -- Focus the designated field
  focus_field(state.ui.focused_field, true)
end

-- Window management
local function create_window()
  local width = 80
  local height = 24

  buf = vim.api.nvim_create_buf(false, true)

  win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = height,
    row = math.floor((vim.o.lines - height) / 2),
    col = math.floor((vim.o.columns - width) / 2),
    style = "minimal",
    border = "rounded",
    title = " Edit Work Item ",
    title_pos = "center",
  })

  -- Buffer options
  vim.api.nvim_buf_set_option(buf, "buftype", "nofile")
  vim.api.nvim_buf_set_option(buf, "bufhidden", "wipe")
  vim.api.nvim_buf_set_option(buf, "modifiable", true)
  vim.api.nvim_buf_set_option(buf, "filetype", "work-form")

  -- Window options
  vim.api.nvim_win_set_option(win, "cursorline", true)
  vim.api.nvim_win_set_option(win, "wrap", true)
  vim.api.nvim_win_set_option(win, "number", false)
  vim.api.nvim_win_set_option(win, "relativenumber", false)
  vim.api.nvim_win_set_option(win, "signcolumn", "no")
  vim.api.nvim_win_set_option(win, "spell", false)

  -- Setup autocmds for text editing
  setup_autocmds()

  -- Setup keymaps
  setup_keymaps()
end

local function close()
  -- Clear autocmds
  if autocmd_group then
    vim.api.nvim_del_augroup_by_id(autocmd_group)
    autocmd_group = nil
  end

  if win and vim.api.nvim_win_is_valid(win) then
    vim.api.nvim_win_close(win, true)
  end
  win = nil
  buf = nil
  state = nil
end

-- Autocmds for text editing
function setup_autocmds()
  autocmd_group = vim.api.nvim_create_augroup("WorkForm_" .. buf, { clear = true })

  -- Sync state when text changes
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI" }, {
    group = autocmd_group,
    buffer = buf,
    callback = function()
      sync_state_from_buffer()
    end,
  })

  -- Constrain cursor to content lines
  vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
    group = autocmd_group,
    buffer = buf,
    callback = function()
      if not win or not vim.api.nvim_win_is_valid(win) then return end
      local cursor = vim.api.nvim_win_get_cursor(win)
      local row = cursor[1]

      local field, pos = get_field_at_line(row)
      if not field then
        -- Cursor is outside a field, move to focused field
        focus_field(state.ui.focused_field)
        return
      end

      -- Check if on label line or blank separator
      if row == pos.start or row == pos["end"] + 1 then
        -- On label or separator, move to first content line
        vim.api.nvim_win_set_cursor(win, { pos.start + 1, cursor[2] })
      end

      -- Update focused field if changed
      if field.name ~= state.ui.focused_field then
        state.ui.focused_field = field.name
        apply_highlights()
      end
    end,
  })
end

-- Capture current state for external operations (pickers, etc)
local function capture_state_for_reopen()
  if not state then return nil end

  sync_state_from_buffer()

  local preserved_item = vim.deepcopy(state.item)
  preserved_item.title = state.data.title
  preserved_item.due = state.data.due
  preserved_item.description = state.data.description
  preserved_item.started = state.data.started
  preserved_item.completed = state.data.completed
  preserved_item.blocks = state.data.blocks
  preserved_item.log = state.data.log
  preserved_item._focused_section = state.ui.focused_field
  preserved_item._autofocus_field = state.ui.focused_field
  preserved_item._logs_sort_asc = state.ui.logs_sort_asc
  preserved_item._original_log = state.original_log
  preserved_item._is_new = state.is_new

  return preserved_item
end

-- Reopen pattern (for internal use - when state is still available)
local function reopen_with_state(extra_fields)
  local preserved_item = capture_state_for_reopen()
  if not preserved_item then return end

  if extra_fields then
    for k, v in pairs(extra_fields) do
      if k:sub(1, 1) == "_" then
        preserved_item[k] = v
      elseif preserved_item[k] ~= nil then
        preserved_item[k] = v
      end
    end
  end

  close()
  vim.schedule(function()
    M.edit(preserved_item)
  end)
end

-- Action handlers
local function add_blocks()
  if not state then return end

  state.ui.focused_field = "blocks"
  local current_blocks = state.data.blocks
  local item_id = state.item.id
  local preserved_item = capture_state_for_reopen()
  close()

  local picker = require("work.picker")
  picker.select_blocks(item_id, function(selected_ids)
    vim.schedule(function()
      if not preserved_item then return end

      if not selected_ids then
        preserved_item._autofocus_field = "blocks"
        M.edit(preserved_item)
        return
      end

      -- Merge with existing blocks
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

      preserved_item.blocks = merged_blocks
      preserved_item._autofocus_field = "blocks"
      M.edit(preserved_item)
    end)
  end)
end

local function delete_blocks()
  if not state then return end

  local current_blocks = state.data.blocks
  if #current_blocks == 0 then
    return
  end

  state.ui.focused_field = "blocks"
  local preserved_item = capture_state_for_reopen()
  close()

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
      vim.schedule(function()
        if not preserved_item then return end

        if not chosen then
          preserved_item._autofocus_field = "blocks"
          M.edit(preserved_item)
          return
        end

        local new_blocks = {}
        for _, block_id in ipairs(current_blocks) do
          if block_id ~= chosen.id then
            table.insert(new_blocks, block_id)
          end
        end

        preserved_item.blocks = new_blocks
        preserved_item._autofocus_field = "blocks"
        M.edit(preserved_item)
      end)
    end,
    choose_marked = function(marked)
      vim.schedule(function()
        if not preserved_item then return end

        if not marked or #marked == 0 then
          preserved_item._autofocus_field = "blocks"
          M.edit(preserved_item)
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

        preserved_item.blocks = new_blocks
        preserved_item._autofocus_field = "blocks"
        M.edit(preserved_item)
      end)
    end,
  }

  local mappings = {
    cancel = {
      char = "<Esc>",
      func = function()
        MiniPick.stop()
        vim.schedule(function()
          if preserved_item then
            preserved_item._autofocus_field = "blocks"
            M.edit(preserved_item)
          end
        end)
      end,
    },
  }

  MiniPick.start({ source = source, window = util.get_window_config(), mappings = mappings })
end

local function add_log_entry()
  if not state then return end

  local preserved_item = capture_state_for_reopen()
  close()
  vim.schedule(function()
    if preserved_item then
      preserved_item._show_log_input = true
      preserved_item._autofocus_field = "log_input"
      M.edit(preserved_item)
    end
  end)
end

local function submit_log_entry()
  if not state then return end

  local message = state.ui.log_input_value
  if message and message ~= "" then
    local timestamp = os.date("%Y-%m-%dT%H:%M:%S")
    local updated_log = vim.deepcopy(state.data.log)
    updated_log[timestamp] = message

    local preserved_item = capture_state_for_reopen()
    close()
    vim.schedule(function()
      if preserved_item then
        preserved_item.log = updated_log
        preserved_item._show_log_input = false
        preserved_item._autofocus_field = "logs"
        M.edit(preserved_item)
      end
    end)
  end
end

local function cancel_log_entry()
  if not state then return end

  local preserved_item = capture_state_for_reopen()
  close()
  vim.schedule(function()
    if preserved_item then
      preserved_item._show_log_input = false
      preserved_item._autofocus_field = "logs"
      M.edit(preserved_item)
    end
  end)
end

local function toggle_logs_sort()
  if not state then return end

  local new_sort = not state.ui.logs_sort_asc
  local preserved_item = capture_state_for_reopen()
  close()
  vim.schedule(function()
    if preserved_item then
      preserved_item._logs_sort_asc = new_sort
      preserved_item._autofocus_field = "logs"
      M.edit(preserved_item)
    end
  end)
end

local function show_delete_confirm()
  if not state then return end

  if state.is_new then
    vim.notify("work: cannot delete unsaved item", vim.log.levels.WARN)
    return
  end

  local preserved_item = capture_state_for_reopen()
  close()
  vim.schedule(function()
    if preserved_item then
      preserved_item._show_delete_confirm = true
      M.edit(preserved_item)
    end
  end)
end

local function confirm_delete()
  if not state then return end

  local file_path = work.get_file_path(state.item.id)
  local item_id = state.item.id
  local short_id = work.short_id(state.item)

  local deleted, del_err = work.delete(item_id)
  if not deleted then
    vim.notify("work: " .. del_err, vim.log.levels.ERROR)
    return
  end
  vim.notify("deleted: " .. short_id)

  local git = require("work.git")
  git.commit(item_id, "delete", file_path)

  close()
end

local function cancel_delete()
  if not state then return end

  local preserved_item = capture_state_for_reopen()
  close()
  vim.schedule(function()
    if preserved_item then
      preserved_item._show_delete_confirm = false
      M.edit(preserved_item)
    end
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
  if not state then return end

  -- Sync final state from buffer
  sync_state_from_buffer()

  if not validators.title(state.data.title) then
    vim.notify("work: title is required", vim.log.levels.ERROR)
    return
  end

  if not validators.due(state.data.due) then
    vim.notify("work: invalid due date format", vim.log.levels.ERROR)
    return
  end

  if not validators.timestamp(state.data.started) then
    vim.notify("work: invalid started timestamp format", vim.log.levels.ERROR)
    return
  end

  if not validators.timestamp(state.data.completed) then
    vim.notify("work: invalid completed timestamp format", vim.log.levels.ERROR)
    return
  end

  local parsed_due = state.data.due
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
  if state.is_new then
    local captured_timestamp = os.date("%Y-%m-%dT%H:%M:%S")
    local new_item, err = work.add(state.data.title, { captured = captured_timestamp })
    if not new_item then
      vim.notify("work: failed to create item: " .. err, vim.log.levels.ERROR)
      return
    end
    state.item.id = new_item.id
    vim.notify("created " .. work.short_id(new_item), vim.log.levels.INFO)

    -- Now update all other fields if they're set
    local updates = {}
    if parsed_due ~= "" then
      updates.due = parsed_due
    end
    if state.data.description ~= "" then
      updates.description = state.data.description
    end
    if state.data.started ~= "" then
      updates.started = state.data.started
    end
    if state.data.completed ~= "" then
      updates.completed = state.data.completed
    end

    if next(updates) then
      local result, update_err = work.update(state.item.id, updates)
      if not result or update_err then
        vim.notify("work: failed to update fields: " .. (update_err or "unknown error"), vim.log.levels.ERROR)
        return
      end
    end

    -- Set blocks if any
    if #state.data.blocks > 0 then
      local result, block_err = work.set_blocks(state.item.id, state.data.blocks)
      if not result or block_err then
        vim.notify("work: failed to set blocks: " .. (block_err or "unknown error"), vim.log.levels.ERROR)
        return
      end
    end

    -- Set logs if any
    if vim.tbl_count(state.data.log) > 0 then
      local result, log_err = work.update(state.item.id, { log = state.data.log })
      if not result or log_err then
        vim.notify("work: failed to set logs: " .. (log_err or "unknown error"), vim.log.levels.ERROR)
        return
      end
    end

    -- Commit the new item to git
    local git = require("work.git")
    git.commit(state.item.id, "add")

    close()
    return
  end

  -- Handle existing item updates
  local updates = {}
  local has_updates = false

  if state.data.title ~= state.original.title then
    updates.title = state.data.title
    has_updates = true
  end

  if parsed_due ~= (state.original.due or "") then
    updates.due = parsed_due == "" and nil or parsed_due
    has_updates = true
  end

  if state.data.description ~= (state.original.description or "") then
    updates.description = state.data.description == "" and nil or state.data.description
    has_updates = true
  end

  if state.data.started ~= (state.original.started or "") then
    updates.started = state.data.started == "" and nil or state.data.started
    has_updates = true
  end

  if state.data.completed ~= (state.original.completed or "") then
    updates.completed = state.data.completed == "" and nil or state.data.completed
    has_updates = true
  end

  local blocks_updated = blocks_changed(state.data.blocks, state.original.blocks or {})
  local logs_updated = logs_changed(state.data.log, state.original.log or {})

  local any_changes = false

  if has_updates then
    local _, err = work.update(state.item.id, updates)
    if not err then
      vim.notify("updated " .. work.short_id(state.item), vim.log.levels.INFO)
      any_changes = true
    else
      vim.notify("work: failed to update: " .. err, vim.log.levels.ERROR)
      return
    end
  end

  if blocks_updated then
    local _, err = work.set_blocks(state.item.id, state.data.blocks)
    if err then
      vim.notify("work: failed to update blocks: " .. err, vim.log.levels.ERROR)
      return
    end
    vim.notify("updated blocks for " .. work.short_id(state.item), vim.log.levels.INFO)
    any_changes = true
  end

  if logs_updated then
    local _, err = work.update(state.item.id, { log = state.data.log })
    if err then
      vim.notify("work: failed to update logs: " .. err, vim.log.levels.ERROR)
      return
    end
    vim.notify("updated logs for " .. work.short_id(state.item), vim.log.levels.INFO)
    any_changes = true
  end

  -- Single git commit for all changes
  if any_changes then
    local git = require("work.git")
    git.commit(state.item.id, "update")
  end

  close()

  if not any_changes then
    vim.notify("no changes made", vim.log.levels.INFO)
  end
end

-- Keymap handlers
local function handle_submit()
  if not state then return end

  if state.ui.show_log_input then
    submit_log_entry()
  else
    submit_form()
  end
end

local function handle_escape()
  if not state then return end

  if state.ui.show_delete_confirm then
    cancel_delete()
  elseif state.ui.show_log_input then
    cancel_log_entry()
  else
    close()
  end
end

local function handle_cancel()
  if not state then return end

  if state.ui.show_delete_confirm then
    cancel_delete()
  elseif state.ui.show_log_input then
    cancel_log_entry()
  else
    close()
  end
end

local function handle_delete_or_field_action()
  if not state then return end

  if state.ui.focused_field == "blocks" then
    delete_blocks()
  else
    show_delete_confirm()
  end
end

local function handle_enter()
  if not state then return end

  if state.ui.show_delete_confirm then
    confirm_delete()
  elseif state.ui.focused_field == "blocks" then
    add_blocks()
  elseif state.ui.focused_field == "logs" and not state.ui.show_log_input then
    add_log_entry()
  elseif state.ui.show_log_input then
    submit_log_entry()
  elseif state.ui.focused_field == "due" then
    -- Open due date picker
    local preserved_item = capture_state_for_reopen()
    close()
    local date_picker = require("work.date_picker")
    date_picker.pick(function(selected_date)
      vim.schedule(function()
        if selected_date and preserved_item then
          preserved_item.due = selected_date
          preserved_item._autofocus_field = "due"
          M.edit(preserved_item)
        elseif preserved_item then
          preserved_item._autofocus_field = "due"
          M.edit(preserved_item)
        end
      end)
    end)
  elseif state.ui.focused_field == "started" then
    -- Open started date picker
    local preserved_item = capture_state_for_reopen()
    close()
    local date_picker = require("work.date_picker")
    date_picker.pick(function(selected_date)
      vim.schedule(function()
        if preserved_item then
          local ts = selected_date and (selected_date .. "T" .. os.date("%H:%M:%S")) or ""
          preserved_item.started = ts
          preserved_item._autofocus_field = "started"
          M.edit(preserved_item)
        end
      end)
    end, { from = -30, to = 0 })
  elseif state.ui.focused_field == "completed" then
    -- Open completed date picker
    local preserved_item = capture_state_for_reopen()
    close()
    local date_picker = require("work.date_picker")
    date_picker.pick(function(selected_date)
      vim.schedule(function()
        if preserved_item then
          local ts = selected_date and (selected_date .. "T" .. os.date("%H:%M:%S")) or ""
          preserved_item.completed = ts
          preserved_item._autofocus_field = "completed"
          M.edit(preserved_item)
        end
      end)
    end, { from = -30, to = 0 })
  end
end

local function handle_submit_or_sort()
  if not state then return end

  if state.ui.focused_field == "logs" then
    toggle_logs_sort()
  else
    submit_form()
  end
end

-- Keymap setup
function setup_keymaps()
  local function map(mode, key, handler)
    vim.keymap.set(mode, key, handler, { buffer = buf, nowait = true, silent = true })
  end

  -- Navigation
  map("n", "<Tab>", focus_next)
  map("n", "<S-Tab>", focus_prev)
  map("n", "<Down>", focus_next)
  map("n", "<Up>", focus_prev)
  map("n", "<C-n>", focus_next)
  map("n", "<C-p>", focus_prev)

  -- Submit
  map("n", "<C-s>", handle_submit)
  map({ "n", "i" }, "<C-CR>", handle_submit)
  map({ "n", "i" }, "<D-CR>", handle_submit)
  map("n", "s", handle_submit_or_sort)

  -- Cancel/Escape
  map("n", "<Esc>", handle_escape)
  map("n", "c", handle_cancel)

  -- Delete
  map("n", "d", handle_delete_or_field_action)
  map("n", "X", show_delete_confirm)
  map("n", "y", function()
    if state.ui.show_delete_confirm then
      confirm_delete()
    end
  end)

  -- Activate current field (open pickers, add blocks, etc.)
  map("n", "<Space>", handle_enter)

  -- mini.jump2d support
  local ok_jump2d, MiniJump2d = pcall(require, "mini.jump2d")
  if ok_jump2d then
    map("n", "f", function()
      MiniJump2d.start(MiniJump2d.builtin_opts.single_character)
    end)
  end
end

-- Public API
function M.edit(item_or_id)
  local item
  local is_new = false

  if item_or_id == nil then
    -- Creating a new item
    is_new = true
    item = {
      id = nil,
      title = "",
      due = nil,
      description = nil,
      started = nil,
      completed = nil,
      blocks = {},
      log = {},
      _is_new = true,
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
    if item._is_new then
      is_new = true
    end
  end

  -- Preserve original log data
  local original_log = item._original_log or vim.deepcopy(item.log or {})
  local original = vim.deepcopy(item)

  -- Initialize state
  state = {
    data = {
      title = item.title or "",
      due = item.due or "",
      description = item.description or "",
      started = item.started or "",
      completed = item.completed or "",
      blocks = item.blocks or {},
      log = item.log or {},
    },
    ui = {
      focused_field = item._autofocus_field or "title",
      show_log_input = item._show_log_input or false,
      show_delete_confirm = item._show_delete_confirm or false,
      logs_sort_asc = item._logs_sort_asc ~= nil and item._logs_sort_asc or false,
      log_input_value = "",
    },
    original = original,
    original_log = original_log,
    is_new = is_new,
    item = item,
    field_positions = {},
  }

  -- Create window and render
  create_window()
  render()
end

return M
