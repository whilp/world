-- luacheck ignore: neovim runtime
--check:false
-- work.nvim - mini.pick integration for work items
local M = {}

local api = require("work").api
local render = require("work.render")
local util = require("work.util")

-- Helper to show log window and reopen picker
local function show_log_and_reopen(item, on_complete)
  local buf, win = util.create_float_window({
    "# log entry for: " .. item.title,
    "# press <CR> or CMD-Enter to save, q to cancel",
    "",
  }, "work log", { height = 8 })

  vim.api.nvim_win_set_cursor(win, {3, 0})
  vim.cmd("startinsert")

  util.setup_float_keymaps(buf, win, function(lines)
    local log_lines = {}
    for _, line in ipairs(lines) do
      if not line:match("^%s*#") and not line:match("^%s*$") then
        table.insert(log_lines, line)
      end
    end

    if #log_lines > 0 then
      local log_message = table.concat(log_lines, "\n")
      local ok, err = api.log(item.id, log_message)
      if ok then
        vim.notify("added log to " .. util.short_id(item))
      else
        vim.notify("work: " .. err, vim.log.levels.ERROR)
      end
    end

    if on_complete then
      on_complete()
    end
  end)
end

-- Format item for picker display
local function format_item(item)
  local status = item.completed and "done" or "todo"
  local item_short_id = util.short_id(item)
  local due = ""
  if item._computed and item._computed.relative_due then
    due = item._computed.relative_due .. " "
  end
  local priority = ""
  if item.priority and item.priority ~= 0 then
    priority = string.format(" [p%d]", item.priority)
  end
  return string.format("[%s] %s %s%s%s", status, item_short_id, due, item.title, priority)
end

-- Create picker source from items
local function make_source(items, name)
  local source_items = {}
  for _, item in ipairs(items) do
    table.insert(source_items, {
      text = format_item(item),
      item = item,
    })
  end
  return {
    items = source_items,
    name = name,
    choose = function(chosen)
      if not chosen then return end
      local path = api.get_file_path(chosen.item.id)
      if path then
        local MiniPick = require("mini.pick")
        local target = MiniPick.get_picker_state().windows.target
        vim.api.nvim_set_current_win(target)
        vim.cmd.edit(path)
      end
    end,
    preview = function(buf_id, chosen)
      if not chosen then return end
      local detail = render.detail(chosen.item)
      local lines = vim.split(detail, "\n")
      vim.api.nvim_buf_set_lines(buf_id, 0, -1, false, lines)
    end,
    choose_marked = function(marked)
      if not marked or #marked == 0 then return end
      local MiniPick = require("mini.pick")
      local target = MiniPick.get_picker_state().windows.target
      vim.api.nvim_set_current_win(target)
      for _, chosen in ipairs(marked) do
        local path = api.get_file_path(chosen.item.id)
        if path then
          vim.cmd.edit(path)
        end
      end
    end,
  }
end

-- Create picker mappings for work actions
-- reopen_fn: optional callback to reopen picker after actions
local function setup_mappings(reopen_fn)
  local MiniPick = require("mini.pick")
  local mappings = {}

  -- mark_done
  mappings.mark_done = {
    char = "<C-d>",
    func = function()
      local matches = MiniPick.get_picker_matches()
      local current = matches.current
      if not current or not current.item then return end
      local ok, err_msg = api.done(current.item.id)
      if ok then
        vim.notify("marked done: " .. util.short_id(current.item))
        MiniPick.stop()
        if reopen_fn then
          vim.defer_fn(reopen_fn, 50)
        end
      else
        vim.notify("work: " .. err_msg, vim.log.levels.ERROR)
      end
    end,
  }

  -- set_due
  mappings.set_due = {
    char = "<C-u>",
    func = function()
      local matches = MiniPick.get_picker_matches()
      local current = matches.current
      if not current or not current.item then return end
      local item_id = current.item.id
      MiniPick.stop()
      vim.ui.input({
        prompt = "Due date (YYYY-MM-DD, 'today', '1d', '2w', etc): ",
        default = current.item.due or ""
      }, function(input)
        if input and input ~= "" then
          local parsed_date = util.parse_relative_date(input)
          if parsed_date then
            local ok, err_msg = api.set_due(item_id, parsed_date)
            if ok then
              vim.notify("set due date: " .. parsed_date)
            else
              vim.notify("work: " .. err_msg, vim.log.levels.ERROR)
            end
          else
            vim.notify("work: invalid date format: " .. input, vim.log.levels.ERROR)
          end
        end
        if reopen_fn then
          vim.defer_fn(reopen_fn, 50)
        end
      end)
    end,
  }

  -- add_log
  mappings.add_log = {
    char = "<C-l>",
    func = function()
      local matches = MiniPick.get_picker_matches()
      local current = matches.current
      if not current or not current.item then return end
      MiniPick.stop()
      show_log_and_reopen(current.item, reopen_fn and function()
        vim.defer_fn(reopen_fn, 50)
      end or nil)
    end,
  }

  -- open_split
  mappings.open_split = {
    char = "<C-s>",
    func = function()
      local matches = MiniPick.get_picker_matches()
      local current = matches.current
      if not current or not current.item then return end
      local path = api.get_file_path(current.item.id)
      if path then
        local target = MiniPick.get_picker_state().windows.target
        vim.api.nvim_set_current_win(target)
        vim.cmd.split(path)
        MiniPick.stop()
      end
    end,
  }

  -- open_vsplit
  mappings.open_vsplit = {
    char = "<C-v>",
    func = function()
      local matches = MiniPick.get_picker_matches()
      local current = matches.current
      if not current or not current.item then return end
      local path = api.get_file_path(current.item.id)
      if path then
        local target = MiniPick.get_picker_state().windows.target
        vim.api.nvim_set_current_win(target)
        vim.cmd.vsplit(path)
        MiniPick.stop()
      end
    end,
  }

  -- open_form
  mappings.open_form = {
    char = "<C-o>",
    func = function()
      local matches = MiniPick.get_picker_matches()
      local current = matches.current
      if not current or not current.item then return end
      MiniPick.stop()
      vim.schedule(function()
        require("work.form").edit(current.item)
      end)
    end,
  }

  -- open_form_enter (default Enter behavior)
  mappings.open_form_enter = {
    char = "<CR>",
    func = function()
      local matches = MiniPick.get_picker_matches()
      local current = matches.current
      if not current or not current.item then return end
      MiniPick.stop()
      vim.schedule(function()
        require("work.form").edit(current.item)
      end)
    end,
  }

  return mappings
end

-- Pick all work items
function M.all()
  local MiniPick = require("mini.pick")
  local items, err = api.get_all()
  if not items then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end
  local source = make_source(items, "Work Items")
  MiniPick.start({ source = source, mappings = setup_mappings(), window = util.get_window_config() })
end

-- Pick ready (unblocked) items
function M.ready()
  local MiniPick = require("mini.pick")
  local items, err = api.get_ready()
  if not items then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end

  local mappings = setup_mappings(M.ready)

  -- Override <C-s> for mark started (instead of split)
  mappings.mark_started = {
    char = "<C-s>",
    func = function()
      local matches = MiniPick.get_picker_matches()
      local current = matches.current
      if not current or not current.item then return end
      local ok, err_msg = api.start(current.item.id)
      if ok then
        vim.notify("marked started: " .. util.short_id(current.item))
        MiniPick.stop()
        vim.defer_fn(M.ready, 50)
      else
        vim.notify("work: " .. err_msg, vim.log.levels.ERROR)
      end
    end,
  }
  mappings.open_split = nil

  local source = make_source(items, "Ready Items")
  MiniPick.start({ source = source, mappings = mappings, window = util.get_window_config() })
end

-- Pick ready started items
function M.ready_started()
  local MiniPick = require("mini.pick")
  local items, err = api.get_ready()
  if not items then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end
  -- Filter for started items
  local started = {}
  for _, item in ipairs(items) do
    if item.started then
      table.insert(started, item)
    end
  end

  local mappings = setup_mappings(M.ready_started)

  local source = make_source(started, "Ready Started Items")
  MiniPick.start({ source = source, mappings = mappings, window = util.get_window_config() })
end

-- Pick blocked items
function M.blocked()
  local MiniPick = require("mini.pick")
  local items, err = api.get_blocked()
  if not items then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end
  local source = make_source(items, "Blocked Items")
  MiniPick.start({ source = source, mappings = setup_mappings(), window = util.get_window_config() })
end

-- Pick incomplete items (todo + blocked)
function M.incomplete()
  local MiniPick = require("mini.pick")
  local items, err = api.get_all()
  if not items then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end
  local incomplete = {}
  for _, item in ipairs(items) do
    if not item.completed then
      table.insert(incomplete, item)
    end
  end
  local source = make_source(incomplete, "Incomplete Items")
  MiniPick.start({ source = source, mappings = setup_mappings(), window = util.get_window_config() })
end

-- Search work items by title/description
function M.search()
  local MiniPick = require("mini.pick")
  MiniPick.builtin.grep_live({
    tool = "rg",
    cwd = DATA_DIR,
  }, {
    source = {
      cwd = DATA_DIR,
    }
  })
end

-- Pick items to add as blocks (with multi-select support)
function M.select_blocks(current_id, callback)
  local MiniPick = require("mini.pick")
  local items, err = api.get_all()
  if not items then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end

  -- Filter out current item and completed items
  local available = {}
  for _, item in ipairs(items) do
    if item.id ~= current_id and not item.completed then
      table.insert(available, item)
    end
  end

  local source_items = {}
  for _, item in ipairs(available) do
    table.insert(source_items, {
      text = format_item(item),
      item = item,
    })
  end

  local callback_called = false

  local source = {
    items = source_items,
    name = "Select blocks",
    choose = function(chosen)
      callback_called = true
      if not chosen then
        callback(nil)
        return
      end
      callback({chosen.item.id})
    end,
    preview = function(buf_id, chosen)
      if not chosen then return end
      local detail = render.detail(chosen.item)
      local lines = vim.split(detail, "\n")
      vim.api.nvim_buf_set_lines(buf_id, 0, -1, false, lines)
    end,
    choose_marked = function(marked)
      callback_called = true
      if not marked or #marked == 0 then
        callback(nil)
        return
      end
      local block_ids = {}
      for _, chosen in ipairs(marked) do
        table.insert(block_ids, chosen.item.id)
      end
      callback(block_ids)
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
