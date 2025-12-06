-- work.nvim - mini.pick integration for work items
local M = {}

local work = require("work")

-- Format item for picker display
local function format_item(item)
  local status = item.completed and "done" or "todo"
  local short_id = work.short_id(item)
  local due = ""
  if item._computed and item._computed.relative_due then
    due = item._computed.relative_due .. " "
  end
  local priority = ""
  if item.priority and item.priority ~= 0 then
    priority = string.format(" [p%d]", item.priority)
  end
  return string.format("[%s] %s %s%s%s", status, short_id, due, item.title, priority)
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
      local path = work.get_file_path(chosen.item.id)
      if path then
        local MiniPick = require("mini.pick")
        local target = MiniPick.get_picker_state().windows.target
        vim.api.nvim_set_current_win(target)
        vim.cmd.edit(path)
      end
    end,
    preview = function(buf_id, chosen)
      if not chosen then return end
      local detail = work.render_detail(chosen.item)
      local lines = vim.split(detail, "\n")
      vim.api.nvim_buf_set_lines(buf_id, 0, -1, false, lines)
    end,
    choose_marked = function(marked)
      if not marked or #marked == 0 then return end
      local MiniPick = require("mini.pick")
      local target = MiniPick.get_picker_state().windows.target
      vim.api.nvim_set_current_win(target)
      for _, chosen in ipairs(marked) do
        local path = work.get_file_path(chosen.item.id)
        if path then
          vim.cmd.edit(path)
        end
      end
    end,
  }
end

-- Create picker mappings for work actions
local function setup_mappings()
  local MiniPick = require("mini.pick")
  return {
    mark_done = {
      char = "<C-d>",
      func = function()
        local matches = MiniPick.get_picker_matches()
        local current = matches.current
        if not current or not current.item then return end
        local ok, err = work.mark_done(current.item.id)
        if ok then
          vim.notify("marked done: " .. work.short_id(current.item))
          MiniPick.stop()
        else
          vim.notify("work: " .. err, vim.log.levels.ERROR)
        end
      end,
    },
    add_log = {
      char = "<C-l>",
      func = function()
        local matches = MiniPick.get_picker_matches()
        local current = matches.current
        if not current or not current.item then return end
        MiniPick.stop()
        vim.ui.input({ prompt = "Log entry: " }, function(message)
          if message and message ~= "" then
            local ok, err = work.add_log(current.item.id, message)
            if ok then
              vim.notify("added log to " .. work.short_id(current.item))
            else
              vim.notify("work: " .. err, vim.log.levels.ERROR)
            end
          end
        end)
      end,
    },
    open_split = {
      char = "<C-s>",
      func = function()
        local matches = MiniPick.get_picker_matches()
        local current = matches.current
        if not current or not current.item then return end
        local path = work.get_file_path(current.item.id)
        if path then
          local target = MiniPick.get_picker_state().windows.target
          vim.api.nvim_set_current_win(target)
          vim.cmd.split(path)
          MiniPick.stop()
        end
      end,
    },
    open_vsplit = {
      char = "<C-v>",
      func = function()
        local matches = MiniPick.get_picker_matches()
        local current = matches.current
        if not current or not current.item then return end
        local path = work.get_file_path(current.item.id)
        if path then
          local target = MiniPick.get_picker_state().windows.target
          vim.api.nvim_set_current_win(target)
          vim.cmd.vsplit(path)
          MiniPick.stop()
        end
      end,
    },
  }
end

-- Pick all work items
function M.all()
  local MiniPick = require("mini.pick")
  local items, err = work.get_all_enriched()
  if not items then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end
  local source = make_source(items, "Work Items")
  MiniPick.start({ source = source, mappings = setup_mappings() })
end

-- Pick ready (unblocked) items
function M.ready()
  local MiniPick = require("mini.pick")
  local items, err = work.get_ready()
  if not items then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end
  local mappings = setup_mappings()

  -- Add mark started mapping with reopen
  mappings.mark_started = {
    char = "<C-t>",
    func = function()
      local matches = MiniPick.get_picker_matches()
      local current = matches.current
      if not current or not current.item then return end
      local ok, err_msg = work.mark_started(current.item.id)
      if ok then
        vim.notify("marked started: " .. work.short_id(current.item))
        MiniPick.stop()
        vim.defer_fn(M.ready, 50)
      else
        vim.notify("work: " .. err_msg, vim.log.levels.ERROR)
      end
    end,
  }

  -- Override mark done to reopen picker
  mappings.mark_done = {
    char = "<C-d>",
    func = function()
      local matches = MiniPick.get_picker_matches()
      local current = matches.current
      if not current or not current.item then return end
      local ok, err_msg = work.mark_done(current.item.id)
      if ok then
        vim.notify("marked done: " .. work.short_id(current.item))
        MiniPick.stop()
        vim.defer_fn(M.ready, 50)
      else
        vim.notify("work: " .. err_msg, vim.log.levels.ERROR)
      end
    end,
  }

  -- Add set due mapping with reopen
  mappings.set_due = {
    char = "<C-u>",
    func = function()
      local matches = MiniPick.get_picker_matches()
      local current = matches.current
      if not current or not current.item then return end
      local item_id = current.item.id
      local item_title = current.item.title
      MiniPick.stop()
      vim.ui.input({
        prompt = "Due date (YYYY-MM-DD, 'today', '1d', '2w', etc): ",
        default = current.item.due or ""
      }, function(input)
        if input and input ~= "" then
          local actions = require("work.actions")
          -- Parse the date using the same logic as actions.set_due
          local function parse_relative_date(date_input)
            if not date_input or date_input == "" then return nil end
            date_input = date_input:lower():gsub("^%s*(.-)%s*$", "%1")
            if date_input == "today" or date_input == "0d" then return os.date("%Y-%m-%d") end
            if date_input == "tomorrow" or date_input == "1d" then return os.date("%Y-%m-%d", os.time() + 86400) end
            local num, unit = date_input:match("^(%d+)([dwmy])$")
            if num and unit then
              num = tonumber(num)
              local seconds_map = { d = 86400, w = 86400 * 7, m = 86400 * 30, y = 86400 * 365 }
              local seconds = seconds_map[unit]
              if seconds then return os.date("%Y-%m-%d", os.time() + (num * seconds)) end
            end
            if date_input:match("^%d%d%d%d%-%d%d%-%d%d$") then return date_input end
            return nil
          end

          local parsed_date = parse_relative_date(input)
          if parsed_date then
            local ok, err_msg = work.set_due(item_id, parsed_date)
            if ok then
              vim.notify("set due date: " .. parsed_date)
            else
              vim.notify("work: " .. err_msg, vim.log.levels.ERROR)
            end
          else
            vim.notify("work: invalid date format: " .. input, vim.log.levels.ERROR)
          end
        end
        vim.defer_fn(M.ready, 50)
      end)
    end,
  }

  -- Override add log to reopen picker
  mappings.add_log = {
    char = "<C-l>",
    func = function()
      local matches = MiniPick.get_picker_matches()
      local current = matches.current
      if not current or not current.item then return end
      local item_id = current.item.id
      MiniPick.stop()
      vim.ui.input({ prompt = "Log entry: " }, function(message)
        if message and message ~= "" then
          local ok, err_msg = work.add_log(item_id, message)
          if ok then
            vim.notify("added log to " .. work.short_id(current.item))
          else
            vim.notify("work: " .. err_msg, vim.log.levels.ERROR)
          end
        end
        vim.defer_fn(M.ready, 50)
      end)
    end,
  }

  local source = make_source(items, "Ready Items")
  MiniPick.start({ source = source, mappings = mappings })
end

-- Pick ready started items
function M.ready_started()
  local MiniPick = require("mini.pick")
  local items, err = work.get_ready()
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

  local mappings = setup_mappings()

  -- Override mark done to reopen picker
  mappings.mark_done = {
    char = "<C-d>",
    func = function()
      local matches = MiniPick.get_picker_matches()
      local current = matches.current
      if not current or not current.item then return end
      local ok, err_msg = work.mark_done(current.item.id)
      if ok then
        vim.notify("marked done: " .. work.short_id(current.item))
        MiniPick.stop()
        vim.defer_fn(M.ready_started, 50)
      else
        vim.notify("work: " .. err_msg, vim.log.levels.ERROR)
      end
    end,
  }

  -- Add set due mapping with reopen
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
          -- Parse the date using the same logic as actions.set_due
          local function parse_relative_date(date_input)
            if not date_input or date_input == "" then return nil end
            date_input = date_input:lower():gsub("^%s*(.-)%s*$", "%1")
            if date_input == "today" or date_input == "0d" then return os.date("%Y-%m-%d") end
            if date_input == "tomorrow" or date_input == "1d" then return os.date("%Y-%m-%d", os.time() + 86400) end
            local num, unit = date_input:match("^(%d+)([dwmy])$")
            if num and unit then
              num = tonumber(num)
              local seconds_map = { d = 86400, w = 86400 * 7, m = 86400 * 30, y = 86400 * 365 }
              local seconds = seconds_map[unit]
              if seconds then return os.date("%Y-%m-%d", os.time() + (num * seconds)) end
            end
            if date_input:match("^%d%d%d%d%-%d%d%-%d%d$") then return date_input end
            return nil
          end

          local parsed_date = parse_relative_date(input)
          if parsed_date then
            local ok, err_msg = work.set_due(item_id, parsed_date)
            if ok then
              vim.notify("set due date: " .. parsed_date)
            else
              vim.notify("work: " .. err_msg, vim.log.levels.ERROR)
            end
          else
            vim.notify("work: invalid date format: " .. input, vim.log.levels.ERROR)
          end
        end
        vim.defer_fn(M.ready_started, 50)
      end)
    end,
  }

  -- Override add log to reopen picker
  mappings.add_log = {
    char = "<C-l>",
    func = function()
      local matches = MiniPick.get_picker_matches()
      local current = matches.current
      if not current or not current.item then return end
      local item_id = current.item.id
      MiniPick.stop()
      vim.ui.input({ prompt = "Log entry: " }, function(message)
        if message and message ~= "" then
          local ok, err_msg = work.add_log(item_id, message)
          if ok then
            vim.notify("added log to " .. work.short_id(current.item))
          else
            vim.notify("work: " .. err_msg, vim.log.levels.ERROR)
          end
        end
        vim.defer_fn(M.ready_started, 50)
      end)
    end,
  }

  local source = make_source(started, "Ready Started Items")
  MiniPick.start({ source = source, mappings = mappings })
end

-- Pick blocked items
function M.blocked()
  local MiniPick = require("mini.pick")
  local items, err = work.get_blocked()
  if not items then
    vim.notify("work: " .. err, vim.log.levels.ERROR)
    return
  end
  local source = make_source(items, "Blocked Items")
  MiniPick.start({ source = source, mappings = setup_mappings() })
end

-- Pick incomplete items (todo + blocked)
function M.incomplete()
  local MiniPick = require("mini.pick")
  local items, err = work.get_all_enriched()
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
  MiniPick.start({ source = source })
end

-- Search work items by title/description
function M.search()
  local MiniPick = require("mini.pick")
  MiniPick.builtin.grep_live({
    tool = "rg",
    cwd = work.config.data_dir,
  }, {
    source = {
      cwd = work.config.data_dir,
    }
  })
end

-- Pick items to add as blocks (with multi-select support)
function M.select_blocks(current_id, callback)
  local MiniPick = require("mini.pick")
  local items, err = work.get_all_enriched()
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

  local source = {
    items = source_items,
    name = "Select blocks",
    choose = function(chosen)
      if not chosen then return end
      callback({chosen.item.id})
    end,
    preview = function(buf_id, chosen)
      if not chosen then return end
      local detail = work.render_detail(chosen.item)
      local lines = vim.split(detail, "\n")
      vim.api.nvim_buf_set_lines(buf_id, 0, -1, false, lines)
    end,
    choose_marked = function(marked)
      if not marked or #marked == 0 then return end
      local block_ids = {}
      for _, chosen in ipairs(marked) do
        table.insert(block_ids, chosen.item.id)
      end
      callback(block_ids)
    end,
  }

  MiniPick.start({ source = source })
end

return M
