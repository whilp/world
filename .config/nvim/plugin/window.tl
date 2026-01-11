-- luacheck ignore: neovim runtime
--check:false
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
    vim.cmd("enew")
    vim.fn.termopen(vim.o.shell, {
      env = vim.tbl_extend("force", vim.fn.environ(), {
        NVIM_BUFFER_ID = unique_id,
        NVIM_SOCKET = vim.v.servername,
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

-- Open file in split above left window (for external editor)
local function open_file_in_left_split(filepath)
  -- Move to leftmost window
  vim.cmd("wincmd h")

  -- Create split above
  local current_win = vim.api.nvim_get_current_win()
  vim.cmd("wincmd k")
  local new_win = vim.api.nvim_get_current_win()

  if current_win == new_win then
    vim.cmd("split")
  end

  -- Open the file
  vim.cmd("edit " .. vim.fn.fnameescape(filepath))
  return vim.api.nvim_get_current_buf()
end

-- Find the maximized window in the target direction
local function find_maximized_window_in_direction(direction)
  local current_win = vim.api.nvim_get_current_win()
  local current_pos = vim.api.nvim_win_get_position(current_win)

  -- Get all windows in current tab
  local windows = vim.api.nvim_tabpage_list_wins(0)

  local target_windows = {}
  for _, win in ipairs(windows) do
    -- Skip floating windows and current window
    local config = vim.api.nvim_win_get_config(win)
    if config.relative == "" and win ~= current_win then
      local pos = vim.api.nvim_win_get_position(win)
      local height = vim.api.nvim_win_get_height(win)

      -- Check if window is in the target direction
      if direction == "left" and pos[2] < current_pos[2] then
        table.insert(target_windows, {win = win, height = height, col = pos[2]})
      elseif direction == "right" and pos[2] > current_pos[2] then
        table.insert(target_windows, {win = win, height = height, col = pos[2]})
      end
    end
  end

  -- Find the maximized window (tallest window)
  local max_height = 0
  local target_win = nil

  for _, info in ipairs(target_windows) do
    if info.height > max_height then
      max_height = info.height
      target_win = info.win
    end
  end

  return target_win
end

local function smart_move_left()
  local target_win = find_maximized_window_in_direction("left")

  if target_win then
    vim.api.nvim_set_current_win(target_win)
  else
    -- Fall back to default behavior if no window found
    vim.cmd("wincmd h")
  end
end

local function smart_move_right()
  local target_win = find_maximized_window_in_direction("right")

  if target_win then
    vim.api.nvim_set_current_win(target_win)
  else
    -- Fall back to default behavior if no window found
    vim.cmd("wincmd l")
  end
end

-- Expose functions globally for use in terminal mode
_G.move_or_split_down = move_or_split_down
_G.move_or_split_up = move_or_split_up
_G.open_file_in_left_split = open_file_in_left_split
_G.smart_move_left = smart_move_left
_G.smart_move_right = smart_move_right

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

-- Auto-maximize current window (but not floating windows)
vim.api.nvim_create_autocmd({ "WinEnter", "BufEnter" }, {
  pattern = "*",
  callback = function()
    local win_config = vim.api.nvim_win_get_config(0)
    if win_config.relative == "" then
      vim.cmd("wincmd _")
    end
  end,
  desc = "Auto-maximize current window",
})

