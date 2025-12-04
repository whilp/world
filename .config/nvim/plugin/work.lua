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
  local cmd = args[1] or "list"
  local rest = table.concat(vim.list_slice(args, 2), " ")

  if cmd == "list" then
    buffer.list()
  elseif cmd == "tree" then
    buffer.tree(rest ~= "" and rest or nil)
  elseif cmd == "ready" then
    buffer.ready()
  elseif cmd == "blocked" then
    buffer.blocked()
  elseif cmd == "show" then
    if rest == "" then
      actions.show()
    else
      buffer.show(rest)
    end
  elseif cmd == "open" then
    if rest == "" then
      actions.open()
    else
      buffer.open(rest)
    end
  elseif cmd == "done" then
    actions.done(rest ~= "" and rest or nil)
  elseif cmd == "add" then
    actions.add(rest ~= "" and rest or nil)
  elseif cmd == "delete" or cmd == "rm" then
    actions.delete(rest ~= "" and rest or nil)
  elseif cmd == "log" then
    actions.log(nil, rest ~= "" and rest or nil)
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
      local cmds = { "list", "tree", "ready", "blocked", "show", "open", "done", "add", "delete", "rm", "log", "pick", "search" }
      return vim.tbl_filter(function(c)
        return c:find(arglead, 1, true) == 1
      end, cmds)
    end
    return {}
  end,
})

-- Keybindings under <Space>w
vim.keymap.set("n", "<Space>wl", function() buffer.list() end, { desc = "Work: list all" })
vim.keymap.set("n", "<Space>wL", function() picker.all() end, { desc = "Work: pick all" })
vim.keymap.set("n", "<Space>wt", function() buffer.tree() end, { desc = "Work: tree view" })
vim.keymap.set("n", "<Space>wr", function() picker.ready() end, { desc = "Work: pick ready" })
vim.keymap.set("n", "<Space>wR", function() buffer.ready() end, { desc = "Work: ready list" })
vim.keymap.set("n", "<Space>wb", function() picker.blocked() end, { desc = "Work: pick blocked" })
vim.keymap.set("n", "<Space>wB", function() buffer.blocked() end, { desc = "Work: blocked list" })
vim.keymap.set("n", "<Space>ws", function() actions.show() end, { desc = "Work: show item" })
vim.keymap.set("n", "<Space>wd", function() actions.done() end, { desc = "Work: mark done" })
vim.keymap.set("n", "<Space>wa", function() actions.add() end, { desc = "Work: add item" })
vim.keymap.set("n", "<Space>wD", function() actions.delete() end, { desc = "Work: delete item" })
vim.keymap.set("n", "<Space>w/", function() picker.search() end, { desc = "Work: search" })

-- gf override for work IDs
vim.keymap.set("n", "gw", function()
  if not buffer.goto_item() then
    vim.notify("work: no work ID under cursor", vim.log.levels.WARN)
  end
end, { desc = "Go to work item" })

