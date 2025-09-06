local opt = vim.opt
local map = vim.keymap.set

-- Window settings
opt.splitright = true
opt.winminheight = 0
opt.winborder = "rounded"
opt.equalalways = false
opt.eadirection = "hor"

-- Window maximizing
map("n", "<Space>z", "<C-w>_", { desc = "Maximize current window height" })

-- Parameterized function to handle window movement and creation
local function handle_window_split(direction, always_create)
  local current_win = vim.api.nvim_get_current_win()

  if always_create then
    -- Always create a new split
    if direction == "down" then
      vim.cmd("belowright split")
      vim.cmd("enew")
    elseif direction == "up" then
      vim.cmd("split")
      vim.cmd("enew")
    end
  else
    -- Move to existing window or create split if none exists
    local move_cmd = direction == "down" and "wincmd j" or "wincmd k"
    vim.cmd(move_cmd)
    local new_win = vim.api.nvim_get_current_win()

    if current_win == new_win then
      -- No window in that direction, create new split
      if direction == "down" then
        vim.cmd("belowright split")
        vim.cmd("enew")
      elseif direction == "up" then
        vim.cmd("split")
        vim.cmd("enew")
      end
    end
  end
  -- Autocmd will handle maximizing the current window
end

-- Convenience functions for backward compatibility
local function move_or_split_down()
  handle_window_split("down", false)
end

local function move_or_split_up()
  handle_window_split("up", false)
end

local function always_split_down()
  handle_window_split("down", true)
end

local function always_split_up()
  handle_window_split("up", true)
end

-- Window navigation and split creation
map("n", "<D-j>", move_or_split_down, { desc = "Move to window below or create split" })
map("n", "<D-k>", move_or_split_up, { desc = "Move to window above or create split" })
map("n", "<D-h>", "<C-w>h", { desc = "Move to window left" })
map("n", "<D-l>", "<C-w>l", { desc = "Move to window right" })

-- Always create new splits
map("n", "<D-S-j>", always_split_down, { desc = "Always create split below and switch to it" })
map("n", "<D-S-k>", always_split_up, { desc = "Always create split above and switch to it" })

map("i", "<D-j>", function()
  move_or_split_down()
end, { desc = "Move to window below or create split" })
map("i", "<D-k>", function()
  move_or_split_up()
end, { desc = "Move to window above or create split" })
map("i", "<D-h>", "<C-o><C-w>h", { desc = "Move to window left" })
map("i", "<D-l>", "<C-o><C-w>l", { desc = "Move to window right" })

-- Always create new splits in insert mode
map("i", "<D-S-j>", function()
  always_split_down()
end, { desc = "Always create split below and switch to it" })
map("i", "<D-S-k>", function()
  always_split_up()
end, { desc = "Always create split above and switch to it" })

-- Buffer management
map("n", "<D-q>", "<cmd>enew|bd #<cr>", { noremap = true, silent = true })

-- Auto-maximize current window
vim.api.nvim_create_autocmd({ "WinEnter", "BufEnter" }, {
  pattern = "*",
  callback = function()
    vim.cmd("wincmd _")
  end,
  desc = "Auto-maximize current window",
})

