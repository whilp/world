-- work.form - native implementation
local work = require("work")
local api = work.api
local render = require("work.render")
local util = require("work.util")

-- Helper to commit changes to git
local function git_commit(id, action, file_path)
  file_path = file_path or api.get_file_path(id)
  if not file_path then
    vim.notify("work: git commit failed - could not get file path for " .. tostring(id), vim.log.levels.ERROR)
    return
  end
  local short_id = id:sub(-6):lower()
  local msg = "work: " .. action .. " " .. short_id

  local add_result = vim.fn.system({"git", "-C", work.config.data_dir, "add", file_path})
  if vim.v.shell_error ~= 0 then
    vim.notify("work: git add failed: " .. add_result, vim.log.levels.ERROR)
    return
  end

  local commit_result = vim.fn.system({"git", "-C", work.config.data_dir, "commit", "-m", msg})
  if vim.v.shell_error ~= 0 then
    -- Check if it's just "nothing to commit" (which is ok)
    if not commit_result:match("nothing to commit") and not commit_result:match("no changes added") then
      vim.notify("work: git commit failed: " .. commit_result, vim.log.levels.WARN)
    end
  end
end

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


local function format_blocks(block_ids)
  if not block_ids or #block_ids == 0 then
    return { "No blocks" }
  end
  local lines = {}
  for _, block_id in ipairs(block_ids) do
    local block_item, _ = api.get(block_id)
    if block_item then
      local label = util.short_id(block_item) .. ": " .. block_item.title
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
    return { "No logs" }
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

-- Update highlight for a single field (avoids full clear/reapply)
local function update_field_highlight(field_name)
  if not buf or not vim.api.nvim_buf_is_valid(buf) then return end
  if not state or not state.field_positions then return end

  local pos = state.field_positions[field_name]
  if not pos then return end

  local is_focused = (field_name == state.ui.focused_field)
  local hl_label = is_focused and "Title" or "Comment"

  -- Clear only this line's highlights
  vim.api.nvim_buf_clear_namespace(buf, ns, pos.start - 1, pos.start)
  -- Re-apply highlight
  vim.api.nvim_buf_add_highlight(buf, ns, hl_label, pos.start - 1, 0, -1)
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

  -- Track old focused field for highlight update
  local old_field = state.ui.focused_field

  -- Update state
  state.ui.focused_field = field_name

  -- Update highlights if not skipping render
  if not skip_render then
    -- Only update the two fields that changed
    update_field_highlight(old_field)
    update_field_highlight(field_name)
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
        value = util.format_due_date(state.data.due)
      elseif field.name == "started" then
        value = util.format_timestamp(state.data.started, "Not started")
      elseif field.name == "completed" then
        value = util.format_timestamp(state.data.completed, "Not completed")
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

  -- Help line
  local help_line
  if state.ui.show_delete_confirm then
    help_line = "Confirm delete? Press Enter to confirm, Esc to cancel"
  else
    help_line = "C-s save · C-q cancel · C-d delete · C-e edit file"
  end
  table.insert(lines, help_line)

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
        local old_field = state.ui.focused_field
        state.ui.focused_field = field.name
        -- Only update highlights for the two fields that changed
        update_field_highlight(old_field)
        update_field_highlight(field.name)
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

local function add_log_entry()
  if not state then return end

  local preserved_item = capture_state_for_reopen()
  close()
  vim.schedule(function()
    if preserved_item then
      preserved_item._show_log_input = true
      preserved_item._autofocus_field = "log_input"
      preserved_item._enter_insert_mode = true
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
        preserved_item._enter_insert_mode = false
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

  local file_path = api.get_file_path(state.item.id)
  local item_id = state.item.id
  local item_short_id = util.short_id(state.item)

  local deleted, del_err = api.delete(item_id)
  if not deleted then
    vim.notify("work: " .. del_err, vim.log.levels.ERROR)
    return
  end
  vim.notify("deleted: " .. item_short_id)

  git_commit(item_id, "delete", file_path)

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

local function edit_file()
  if not state then return end

  if state.is_new then
    vim.notify("work: save item before editing file", vim.log.levels.WARN)
    return
  end

  local file_path = api.get_file_path(state.item.id)
  if not file_path then
    vim.notify("work: file not found", vim.log.levels.ERROR)
    return
  end

  close()
  vim.schedule(function()
    vim.cmd.edit(file_path)
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
    local new_item, err = api.add(state.data.title, { captured = captured_timestamp })
    if not new_item then
      vim.notify("work: failed to create item: " .. err, vim.log.levels.ERROR)
      return
    end
    state.item.id = new_item.id
    vim.notify("created " .. util.short_id(new_item), vim.log.levels.INFO)

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
      local result, update_err = api.update(state.item.id, updates)
      if not result or update_err then
        vim.notify("work: failed to update fields: " .. (update_err or "unknown error"), vim.log.levels.ERROR)
        return
      end
    end

    -- Set blocks if any
    if #state.data.blocks > 0 then
      local result, block_err = api.set_blocks(state.item.id, state.data.blocks)
      if not result or block_err then
        vim.notify("work: failed to set blocks: " .. (block_err or "unknown error"), vim.log.levels.ERROR)
        return
      end
    end

    -- Set logs if any
    if vim.tbl_count(state.data.log) > 0 then
      local result, log_err = api.update(state.item.id, { log = state.data.log })
      if not result or log_err then
        vim.notify("work: failed to set logs: " .. (log_err or "unknown error"), vim.log.levels.ERROR)
        return
      end
    end

    -- Commit the new item to git
    git_commit(state.item.id, "add")

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
    local _, err = api.update(state.item.id, updates)
    if not err then
      vim.notify("updated " .. util.short_id(state.item), vim.log.levels.INFO)
      any_changes = true
    else
      vim.notify("work: failed to update: " .. err, vim.log.levels.ERROR)
      return
    end
  end

  if blocks_updated then
    local _, err = api.set_blocks(state.item.id, state.data.blocks)
    if err then
      vim.notify("work: failed to update blocks: " .. err, vim.log.levels.ERROR)
      return
    end
    vim.notify("updated blocks for " .. util.short_id(state.item), vim.log.levels.INFO)
    any_changes = true
  end

  if logs_updated then
    local _, err = api.update(state.item.id, { log = state.data.log })
    if err then
      vim.notify("work: failed to update logs: " .. err, vim.log.levels.ERROR)
      return
    end
    vim.notify("updated logs for " .. util.short_id(state.item), vim.log.levels.INFO)
    any_changes = true
  end

  -- Single git commit for all changes
  if any_changes then
    git_commit(state.item.id, "update")
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

local function handle_delete_item()
  if not state then return end
  show_delete_confirm()
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

-- Keymap setup
function setup_keymaps()
  local function map(mode, key, handler)
    vim.keymap.set(mode, key, handler, { buffer = buf, nowait = true, silent = true })
  end

  -- Insert mode trigger - contextual based on field type
  local function handle_insert_mode()
    if not state then return end
    local field_def = get_field_def(state.ui.focused_field)
    if field_def and field_def.type == "text_input" then
      -- Allow normal insert mode for text fields
      return false
    else
      -- Trigger action for button/paragraph fields
      handle_enter()
      return true
    end
  end

  -- Navigation
  map("n", "<Tab>", focus_next)
  map("n", "<S-Tab>", focus_prev)
  map("n", "<Down>", focus_next)
  map("n", "<Up>", focus_prev)
  map("n", "<C-n>", focus_next)
  map("n", "<C-p>", focus_prev)

  -- Insert mode keys - contextual
  for _, key in ipairs({"i", "a", "o", "I", "A", "O"}) do
    map("n", key, function()
      if not handle_insert_mode() then
        -- Let the key through for text fields
        local feedkey = vim.api.nvim_replace_termcodes(key, true, false, true)
        vim.api.nvim_feedkeys(feedkey, "n", false)
      end
    end)
  end

  -- Submit
  map("n", "<C-s>", handle_submit)
  map({ "n", "i" }, "<C-CR>", handle_submit)
  map({ "n", "i" }, "<D-CR>", handle_submit)

  -- Sort logs (only on logs field)
  map("n", "s", function()
    if state and state.ui.focused_field == "logs" then
      toggle_logs_sort()
    end
  end)

  -- Cancel/Escape
  map("n", "<Esc>", handle_escape)
  map({ "n", "i" }, "<C-q>", handle_cancel)

  -- Delete item
  map("n", "<C-d>", handle_delete_item)

  -- Delete field content (dd in vim style)
  map("n", "dd", function()
    if not state then return end

    if state.ui.focused_field == "blocks" then
      -- Delete the block under cursor
      if not win or not vim.api.nvim_win_is_valid(win) then return end
      local cursor = vim.api.nvim_win_get_cursor(win)
      local row = cursor[1]

      local pos = state.field_positions["blocks"]
      if not pos then return end

      -- Calculate which block line we're on (1-indexed)
      local block_line_idx = row - pos.start  -- pos.start is label line

      if block_line_idx > 0 and block_line_idx <= #state.data.blocks then
        -- Remove the block at this index
        table.remove(state.data.blocks, block_line_idx)
        render()
      end
    elseif state.ui.focused_field == "due" then
      state.data.due = ""
      render()
    elseif state.ui.focused_field == "started" then
      state.data.started = ""
      render()
    elseif state.ui.focused_field == "completed" then
      state.data.completed = ""
      render()
    end
  end)

  -- Edit file directly
  map("n", "<C-e>", edit_file)

  -- Activate current field (open pickers, add blocks, etc.) or confirm delete
  map("n", "<Space>", handle_enter)

  -- Enter for confirming delete
  map("n", "<CR>", function()
    if state and state.ui.show_delete_confirm then
      confirm_delete()
    end
  end)

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
    local loaded_item, err = api.get(item_or_id)
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

  -- Handle insert mode based on flags
  if item._enter_insert_mode then
    -- Auto-enter insert mode if requested (e.g., for log input)
    vim.schedule(function()
      if win and vim.api.nvim_win_is_valid(win) then
        vim.cmd("startinsert")
      end
    end)
  elseif item._enter_insert_mode == false then
    -- Explicitly ensure normal mode
    vim.schedule(function()
      if win and vim.api.nvim_win_is_valid(win) then
        vim.cmd("stopinsert")
      end
    end)
  end
end

return M
