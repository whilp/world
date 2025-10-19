vim.pack.add({
  { src = "https://github.com/nvim-mini/mini.nvim" },
})

-- Setup mini.nvim modules individually (don't require 'mini' directly)
local ok_bufremove, _ = pcall(require, "mini.bufremove")
if ok_bufremove then
  require("mini.bufremove").setup()
end

local ok_diff, _ = pcall(require, "mini.diff")
if ok_diff then
  require("mini.diff").setup()
end

local ok_git, _ = pcall(require, "mini.git")
if ok_git then
  require("mini.git").setup()
end

local ok_pick, _ = pcall(require, "mini.pick")
if ok_pick then
  require("mini.pick").setup()
end

local ok_extra, _ = pcall(require, "mini.extra")
if ok_extra then
  require("mini.extra").setup()
end

-- Setup mini.hues color scheme
local ok_hues, _ = pcall(require, "mini.hues")
if ok_hues then

  -- Function to convert string to deterministic hue (0-359)
  local function string_to_hue(str)
    local hash = 0
    for i = 1, #str do
      hash = (hash * 31 + string.byte(str, i)) % 2147483647
    end
    return hash % 360
  end

  -- Get host identifier and generate deterministic color scheme
  local whereami = require('whereami')
  local host_id = whereami.get()
  local hue = string_to_hue(host_id)

  local base_colors = require("mini.hues").gen_random_base_colors({
    gen_hue = function() return hue end
  })

  require("mini.hues").setup({
    background = base_colors.background,
    foreground = base_colors.foreground,
  })

end

-- mini.pick keybindings (only if mini.pick is available)
if ok_pick then
  vim.keymap.set('n', '<Space>pf', '<Cmd>Pick files<CR>', { desc = 'Pick files' })
  vim.keymap.set('n', '<Space>pg', '<Cmd>Pick git_files<CR>', { desc = 'Pick git files' })
  vim.keymap.set('n', '<Space>pb', '<Cmd>Pick buffers<CR>', { desc = 'Pick buffers' })
  vim.keymap.set('n', '<Space>ph', '<Cmd>Pick help<CR>', { desc = 'Pick help' })
  vim.keymap.set('n', '<Space>pr', '<Cmd>Pick resume<CR>', { desc = 'Resume last pick' })
  vim.keymap.set('n', '<Space>p/', '<Cmd>Pick grep_live<CR>', { desc = 'Live grep' })
end
