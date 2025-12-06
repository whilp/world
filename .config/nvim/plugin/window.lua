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

-- Create appropriate buffer based on current context
local function create_contextual_buffer()
  local current_buf = vim.api.nvim_get_current_buf()
  local is_terminal = vim.bo[current_buf].buftype == "terminal"

  if is_terminal then
    local buf_id = vim.api.nvim_get_current_buf()
    local unique_id = "nvim_" .. buf_id .. "_" .. os.time()
    vim.fn.termopen(vim.o.shell, {
      env = vim.tbl_extend("force", vim.fn.environ(), {
        NVIM_BUFFER_ID = unique_id,
        NVIM_SOCKET = "~/.config/nvim/nvim.sock",
      }),
    })
  else
    vim.cmd("enew")
  end
end

-- Parameterized function to handle window movement and creation
local function handle_window_split(direction, always_create)
  local current_win = vim.api.nvim_get_current_win()
  local split_cmd = direction == "down" and "belowright split" or "split"

  if always_create then
    vim.cmd(split_cmd)
    create_contextual_buffer()
  else
    -- Move to existing window or create split if none exists
    local move_cmd = direction == "down" and "wincmd j" or "wincmd k"
    vim.cmd(move_cmd)
    local new_win = vim.api.nvim_get_current_win()

    if current_win == new_win then
      -- No window in that direction, create new split
      vim.cmd(split_cmd)
      create_contextual_buffer()
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

-- Track last window per column for smart horizontal navigation
local last_left_window = nil
local last_right_window = nil

local function smart_move_left()
  local current_win = vim.api.nvim_get_current_win()

  -- Store current window as last right window
  last_right_window = current_win

  -- Try to return to the last left window if it still exists
  if last_left_window and vim.api.nvim_win_is_valid(last_left_window) then
    vim.api.nvim_set_current_win(last_left_window)
  else
    -- Fall back to default behavior
    vim.cmd("wincmd h")
    last_left_window = vim.api.nvim_get_current_win()
  end
end

local function smart_move_right()
  local current_win = vim.api.nvim_get_current_win()

  -- Store current window as last left window
  last_left_window = current_win

  -- Try to return to the last right window if it still exists
  if last_right_window and vim.api.nvim_win_is_valid(last_right_window) then
    vim.api.nvim_set_current_win(last_right_window)
  else
    -- Fall back to default behavior
    vim.cmd("wincmd l")
    last_right_window = vim.api.nvim_get_current_win()
  end
end

-- Window navigation and split creation
map("n", "<D-j>", move_or_split_down, { desc = "Move to window below or create split" })
map("n", "<D-k>", move_or_split_up, { desc = "Move to window above or create split" })
map("n", "<D-h>", smart_move_left, { desc = "Move to window left" })
map("n", "<D-l>", smart_move_right, { desc = "Move to window right" })

-- Always create new splits
map("n", "<D-S-j>", always_split_down, { desc = "Always create split below and switch to it" })
map("n", "<D-S-k>", always_split_up, { desc = "Always create split above and switch to it" })

map("i", "<D-j>", function()
  move_or_split_down()
end, { desc = "Move to window below or create split" })
map("i", "<D-k>", function()
  move_or_split_up()
end, { desc = "Move to window above or create split" })
map("i", "<D-h>", function()
  smart_move_left()
end, { desc = "Move to window left" })
map("i", "<D-l>", function()
  smart_move_right()
end, { desc = "Move to window right" })

-- Always create new splits in insert mode
map("i", "<D-S-j>", function()
  always_split_down()
end, { desc = "Always create split below and switch to it" })
map("i", "<D-S-k>", function()
  always_split_up()
end, { desc = "Always create split above and switch to it" })

-- Buffer management
map("n", "<D-q>", "<cmd>enew|bd #<cr>", { noremap = true, silent = true, desc = "Close current buffer" })

-- Auto-maximize current window
vim.api.nvim_create_autocmd({ "WinEnter", "BufEnter" }, {
  pattern = "*",
  callback = function()
    vim.cmd("wincmd _")
  end,
  desc = "Auto-maximize current window",
})

