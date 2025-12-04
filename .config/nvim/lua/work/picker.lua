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
  local source = make_source(items, "Ready Items")
  MiniPick.start({ source = source, mappings = setup_mappings() })
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

return M
