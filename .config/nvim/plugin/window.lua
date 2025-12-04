-- window.lua: region-based window management
-- Two regions: left and right
-- Windows stack vertically within each region

local opt = vim.opt
local map = vim.keymap.set

opt.splitright = true
opt.splitbelow = true
opt.winminheight = 0
opt.winminwidth = 0
opt.equalalways = false

local M = {}
M.last_active = { left = nil, right = nil }

local function get_window_info(winid)
  local pos = vim.api.nvim_win_get_position(winid)
  return {
    row = pos[1],
    col = pos[2],
    width = vim.api.nvim_win_get_width(winid),
    height = vim.api.nvim_win_get_height(winid),
    winid = winid,
  }
end

local function get_tab_windows()
  local wins = vim.api.nvim_tabpage_list_wins(0)
  local infos = {}
  for _, w in ipairs(wins) do
    table.insert(infos, get_window_info(w))
  end
  return infos
end

local function get_window_region(win_info)
  local midpoint = math.floor(vim.o.columns / 2)
  return win_info.col < midpoint and "left" or "right"
end

local function get_current_region()
  local win = vim.api.nvim_get_current_win()
  return get_window_region(get_window_info(win))
end

local function get_windows_in_region(region)
  local all = get_tab_windows()
  local result = {}
  for _, w in ipairs(all) do
    if get_window_region(w) == region then
      table.insert(result, w)
    end
  end
  -- Sort by row (top to bottom)
  table.sort(result, function(a, b) return a.row < b.row end)
  return result
end

local function maximize_in_region()
  local current_win = vim.api.nvim_get_current_win()
  local region = get_current_region()
  local region_windows = get_windows_in_region(region)

  -- Minimize all other windows in region
  for _, w in ipairs(region_windows) do
    if w.winid ~= current_win then
      vim.api.nvim_win_set_height(w.winid, 0)
    end
  end

  -- Maximize current window
  vim.cmd("resize")

  M.last_active[region] = current_win
end

local function navigate(direction)
  local current_win = vim.api.nvim_get_current_win()
  local region = get_current_region()
  local region_windows = get_windows_in_region(region)

  -- Find current window index
  local current_idx = nil
  for i, w in ipairs(region_windows) do
    if w.winid == current_win then
      current_idx = i
      break
    end
  end

  if not current_idx then
    return
  end

  local target_idx = direction == "j" and current_idx + 1 or current_idx - 1

  if target_idx >= 1 and target_idx <= #region_windows then
    -- Navigate to existing window
    vim.api.nvim_set_current_win(region_windows[target_idx].winid)
    maximize_in_region()
  else
    -- At boundary - create new split
    if direction == "j" then
      vim.cmd("belowright split")
    else
      vim.cmd("aboveleft split")
    end
    vim.cmd("enew")
    maximize_in_region()
  end
end

local function switch_region()
  local current_win = vim.api.nvim_get_current_win()
  local region = get_current_region()
  M.last_active[region] = current_win

  local other_region = region == "left" and "right" or "left"
  local other_windows = get_windows_in_region(other_region)

  if #other_windows > 0 then
    -- Go to last active or first window in other region
    local target = M.last_active[other_region]
    if not target or not vim.api.nvim_win_is_valid(target) then
      target = other_windows[1].winid
    end
    vim.api.nvim_set_current_win(target)
    maximize_in_region()
  else
    -- Create new region
    vim.cmd("belowright vsplit")
    vim.cmd("enew")
    maximize_in_region()
  end
end

-- Keymaps
map({ "n", "i" }, "<D-j>", function() navigate("j") end, { silent = true, desc = "Move down/split" })
map({ "n", "i" }, "<D-k>", function() navigate("k") end, { silent = true, desc = "Move up/split" })
map({ "n", "i" }, "<D-i>", switch_region, { silent = true, desc = "Switch region" })
map({ "n", "i" }, "<D-h>", switch_region, { silent = true, desc = "Switch region" })
map({ "n", "i" }, "<D-l>", switch_region, { silent = true, desc = "Switch region" })
map("n", "<D-q>", "<cmd>enew|bd #<cr>", { desc = "Close buffer" })
map("n", "<Space>z", maximize_in_region, { desc = "Maximize in region" })

-- Debug
map("n", "<Space>d", function()
  local wins = get_tab_windows()
  table.sort(wins, function(a, b) return a.col < b.col or (a.col == b.col and a.row < b.row) end)
  local lines = {}
  for i, w in ipairs(wins) do
    local region = get_window_region(w)
    table.insert(lines, string.format("win%d: col=%d row=%d h=%d region=%s", i, w.col, w.row, w.height, region))
  end
  vim.notify(table.concat(lines, "\n"))
end, { desc = "Debug layout" })
