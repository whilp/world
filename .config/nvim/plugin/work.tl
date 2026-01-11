-- luacheck ignore: neovim runtime
--check:false
-- work.nvim - plugin entry point
-- keybindings and commands for work todo system

local ok, _ = pcall(require, "work")
if not ok then
  return
end

local work = require("work")
local picker = require("work.picker")
local buffer = require("work.buffer")
local actions = require("work.actions")

-- Commands
vim.api.nvim_create_user_command("Work", function(opts)
  local args = vim.split(opts.args, "%s+")
  local cmd = args[1] or "pick"
  local rest = table.concat(vim.list_slice(args, 2), " ")

  if cmd == "started" then
    picker.ready_started()
  elseif cmd == "show" then
    actions.show(rest ~= "" and rest or nil)
  elseif cmd == "open" then
    if rest == "" then
      actions.open()
    else
      buffer.open(rest)
    end
  elseif cmd == "done" then
    actions.done(rest ~= "" and rest or nil)
  elseif cmd == "capture" then
    actions.quick_capture()
  elseif cmd == "delete" or cmd == "rm" then
    actions.delete(rest ~= "" and rest or nil)
  elseif cmd == "log" then
    actions.log(nil, rest ~= "" and rest or nil)
  elseif cmd == "due" then
    actions.set_due(nil, rest ~= "" and rest or nil)
  elseif cmd == "pick" then
    picker.all()
  elseif cmd == "search" then
    picker.search()
  else
    vim.notify("work: unknown command: " .. cmd, vim.log.levels.ERROR)
  end
end, {
  nargs = "*",
  complete = function(arglead, cmdline, cursorpos)
    local args = vim.split(cmdline, "%s+")
    if #args <= 2 then
      local cmds = { "started", "show", "open", "done", "capture", "delete", "rm", "log", "due", "pick", "search" }
      return vim.tbl_filter(function(c)
        return c:find(arglead, 1, true) == 1
      end, cmds)
    end
    return {}
  end,
})

vim.api.nvim_create_user_command("WorkEdit", function(opts)
  require("work.form").edit(opts.args ~= "" and opts.args or nil)
end, { nargs = "?", desc = "Edit work item in form" })

-- Keybindings under <Space>w
vim.keymap.set("n", "<Space>wl", function() actions.log() end, { desc = "Work: log entry" })
vim.keymap.set("n", "<Space>wL", function() picker.all() end, { desc = "Work: pick all" })
vim.keymap.set("n", "<Space>wr", function() picker.ready() end, { desc = "Work: pick ready" })
vim.keymap.set("n", "<Space>ws", function() picker.ready_started() end, { desc = "Work: pick ready started" })
vim.keymap.set("n", "<Space>wS", function() actions.show() end, { desc = "Work: show item" })
vim.keymap.set("n", "<Space>wb", function() actions.add_blocks() end, { desc = "Work: add blocks" })
vim.keymap.set("n", "<Space>wB", function() picker.blocked() end, { desc = "Work: pick blocked" })
vim.keymap.set("n", "<Space>wd", function() actions.done() end, { desc = "Work: mark done" })
vim.keymap.set("n", "<Space>wu", function() actions.set_due() end, { desc = "Work: set due date" })
vim.keymap.set("n", "<Space>wc", function() require("work.form").edit() end, { desc = "Work: create item" })
vim.keymap.set("n", "<Space>wD", function() actions.delete() end, { desc = "Work: delete item" })
vim.keymap.set("n", "<Space>w/", function() picker.search() end, { desc = "Work: search" })

-- gf override for work IDs
vim.keymap.set("n", "gw", function()
  if not buffer.goto_item() then
    vim.notify("work: no work ID under cursor", vim.log.levels.WARN)
  end
end, { desc = "Go to work item" })

-- Setup work submode with mini.clue
local function setup_work_submode()
  local ok_clue, clue = pcall(require, "mini.clue")
  if not ok_clue then return end

  -- Add work submode trigger
  table.insert(clue.config.triggers, { mode = 'n', keys = '<Space>w' })

  -- Add clues with postkeys to stay in submode
  -- Note: pickers and interactive UIs don't use postkeys to avoid input interference
  local work_clues = {
    { mode = 'n', keys = '<Space>wl', desc = 'Work: log entry' },
    { mode = 'n', keys = '<Space>wL', desc = 'Work: pick all' },
    { mode = 'n', keys = '<Space>wr', desc = 'Work: pick ready' },
    { mode = 'n', keys = '<Space>ws', desc = 'Work: pick ready started' },
    { mode = 'n', keys = '<Space>wS', desc = 'Work: show item', postkeys = '<Space>w' },
    { mode = 'n', keys = '<Space>wb', desc = 'Work: add blocks' },
    { mode = 'n', keys = '<Space>wB', desc = 'Work: pick blocked' },
    { mode = 'n', keys = '<Space>wd', desc = 'Work: mark done', postkeys = '<Space>w' },
    { mode = 'n', keys = '<Space>wu', desc = 'Work: set due date' },
    { mode = 'n', keys = '<Space>wc', desc = 'Work: create item' },
    { mode = 'n', keys = '<Space>wD', desc = 'Work: delete item', postkeys = '<Space>w' },
    { mode = 'n', keys = '<Space>w/', desc = 'Work: search' },
  }

  for _, clue_item in ipairs(work_clues) do
    table.insert(clue.config.clues, clue_item)
  end
end

setup_work_submode()
