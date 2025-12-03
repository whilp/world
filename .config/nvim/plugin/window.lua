local opt = vim.opt
local map = vim.keymap.set

-- Window settings
opt.splitright = true
opt.splitbelow = true
opt.winminheight = 0
opt.winminwidth = 0
opt.equalalways = false

-- Window manager state
local M = {}

-- Detect if nvim is in tall or wide mode
-- Monospace chars are ~2:1 (height:width), so we adjust for that
local function get_layout_mode()
  local cols = vim.o.columns
  local rows = vim.o.lines
  -- If half the columns is greater than rows, we're in wide mode
  return (cols / 2) > rows and "wide" or "tall"
end

-- Get window position info
local function get_window_info(winid)
  local pos = vim.api.nvim_win_get_position(winid)
  local width = vim.api.nvim_win_get_width(winid)
  local height = vim.api.nvim_win_get_height(winid)
  return {
    row = pos[1],
    col = pos[2],
    width = width,
    height = height,
    winid = winid
  }
end

-- Get all windows in current tab
local function get_tab_windows()
  local windows = vim.api.nvim_tabpage_list_wins(0)
  local window_infos = {}
  for _, winid in ipairs(windows) do
    table.insert(window_infos, get_window_info(winid))
  end
  return window_infos
end

-- Determine which region a window belongs to
local function get_window_region(window_info, mode)
  local total_height = vim.o.lines - vim.o.cmdheight - 1 -- subtract statusline
  local total_width = vim.o.columns

  if mode == "tall" then
    local midpoint = math.floor(total_height / 2)
    return window_info.row < midpoint and "top" or "bottom"
  else -- wide
    local midpoint = math.floor(total_width / 2)
    return window_info.col < midpoint and "left" or "right"
  end
end

-- Get current window's region
local function get_current_region()
  local mode = get_layout_mode()
  local current_win = vim.api.nvim_get_current_win()
  local win_info = get_window_info(current_win)
  return get_window_region(win_info, mode), mode
end

-- Find windows in a specific region
local function get_windows_in_region(region, mode)
  local all_windows = get_tab_windows()
  local region_windows = {}

  for _, win_info in ipairs(all_windows) do
    if get_window_region(win_info, mode) == region then
      table.insert(region_windows, win_info)
    end
  end

  return region_windows
end

-- Find the next window in a direction within the current region
local function find_next_window_in_region(direction, current_win_info, region, mode)
  local region_windows = get_windows_in_region(region, mode)
  local current_row = current_win_info.row
  local current_col = current_win_info.col

  local best_window = nil
  local best_distance = math.huge

  for _, win_info in ipairs(region_windows) do
    if win_info.winid ~= current_win_info.winid then
      local is_valid = false
      local distance = 0

      if mode == "tall" then
        -- In tall mode, j/k move vertically
        if direction == "j" and win_info.row > current_row then
          is_valid = true
          distance = win_info.row - current_row
        elseif direction == "k" and win_info.row < current_row then
          is_valid = true
          distance = current_row - win_info.row
        end
      else
        -- In wide mode, j/k still move vertically within the column
        if direction == "j" and win_info.row > current_row then
          is_valid = true
          distance = win_info.row - current_row
        elseif direction == "k" and win_info.row < current_row then
          is_valid = true
          distance = current_row - win_info.row
        end
      end

      if is_valid and distance < best_distance then
        best_distance = distance
        best_window = win_info
      end
    end
  end

  return best_window
end

-- Maximize current window based on mode
local function maximize_current_window()
  local mode = get_layout_mode()

  if mode == "tall" then
    -- In tall mode, maximize width (windows are side-by-side)
    local available_width = vim.o.columns
    vim.api.nvim_win_set_width(0, available_width)
  else
    -- In wide mode, maximize height (windows are stacked vertically)
    local available_height = vim.o.lines - vim.o.cmdheight - 1
    vim.api.nvim_win_set_height(0, available_height)
  end
end

-- Handle navigation with j/k
local function handle_navigation(direction)
  local current_win = vim.api.nvim_get_current_win()
  local current_win_info = get_window_info(current_win)
  local region, mode = get_current_region()

  -- Try to find next window in same region
  local next_window = find_next_window_in_region(direction, current_win_info, region, mode)

  if next_window then
    -- Move to existing window
    vim.api.nvim_set_current_win(next_window.winid)
    maximize_current_window()
  else
    -- No window found, create new split at edge of region
    if mode == "tall" then
      -- In tall mode, create vsplits (left/right)
      if direction == "j" then
        vim.cmd("belowright vsplit")
      else -- k
        vim.cmd("aboveleft vsplit")
      end
    else
      -- In wide mode, create splits (top/bottom)
      if direction == "j" then
        vim.cmd("belowright split")
      else -- k
        vim.cmd("aboveleft split")
      end
    end
    vim.cmd("enew")
    maximize_current_window()
  end
end

-- Switch to other region
local function switch_region()
  local region, mode = get_current_region()
  local other_region

  if mode == "tall" then
    other_region = region == "top" and "bottom" or "top"
  else
    other_region = region == "left" and "right" or "left"
  end

  local other_windows = get_windows_in_region(other_region, mode)

  if #other_windows > 0 then
    -- Find the closest window in the other region
    local current_win_info = get_window_info(vim.api.nvim_get_current_win())
    local best_window = other_windows[1]
    local best_distance = math.huge

    for _, win_info in ipairs(other_windows) do
      local distance
      if mode == "tall" then
        -- In tall mode, prefer window with similar column position
        distance = math.abs(win_info.col - current_win_info.col)
      else
        -- In wide mode, prefer window with similar row position
        distance = math.abs(win_info.row - current_win_info.row)
      end

      if distance < best_distance then
        best_distance = distance
        best_window = win_info
      end
    end

    vim.api.nvim_set_current_win(best_window.winid)
    maximize_current_window()
  else
    -- No window in other region, create one
    if mode == "tall" then
      if other_region == "bottom" then
        vim.cmd("belowright split")
      else
        vim.cmd("aboveleft split")
      end
    else
      if other_region == "right" then
        vim.cmd("belowright vsplit")
      else
        vim.cmd("aboveleft vsplit")
      end
    end
    vim.cmd("enew")
    maximize_current_window()
  end
end

-- Key mappings
map("n", "<D-j>", function() handle_navigation("j") end, { desc = "Move down or create split" })
map("n", "<D-k>", function() handle_navigation("k") end, { desc = "Move up or create split" })
map("n", "<D-i>", switch_region, { desc = "Switch to other region" })

map("i", "<D-j>", function() handle_navigation("j") end, { desc = "Move down or create split" })
map("i", "<D-k>", function() handle_navigation("k") end, { desc = "Move up or create split" })
map("i", "<D-i>", switch_region, { desc = "Switch to other region" })

-- Horizontal navigation (basic)
map("n", "<D-h>", "<C-w>h", { desc = "Move to window left" })
map("n", "<D-l>", "<C-w>l", { desc = "Move to window right" })
map("i", "<D-h>", "<C-o><C-w>h", { desc = "Move to window left" })
map("i", "<D-l>", "<C-o><C-w>l", { desc = "Move to window right" })

-- Buffer management
map("n", "<D-q>", "<cmd>enew|bd #<cr>", { desc = "Close current buffer" })

-- Manual maximize
map("n", "<Space>z", maximize_current_window, { desc = "Maximize current window" })

-- Auto-maximize current window on entry (disabled for testing)
-- vim.api.nvim_create_autocmd({ "WinEnter", "BufEnter" }, {
--   pattern = "*",
--   callback = function()
--     vim.schedule(maximize_current_window)
--   end,
--   desc = "Auto-maximize current window",
-- })
