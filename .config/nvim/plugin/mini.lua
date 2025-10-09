vim.pack.add({
  { src = "https://github.com/nvim-mini/mini.nvim" },
})

local ok, mini = pcall(require, "mini")
if ok then
  require("mini.bufremove").setup()
  require("mini.diff").setup()
  require("mini.git").setup()
  require("mini.pick").setup()
  require("mini.extra").setup()
  require("mini.hues").setup({
    background = '#0d1117',
    foreground = '#c9d1d9',
  })

  -- mini.pick keybindings
  vim.keymap.set('n', '<Space>pf', '<Cmd>Pick files<CR>', { desc = 'Pick files' })
  vim.keymap.set('n', '<Space>pg', '<Cmd>Pick git_files<CR>', { desc = 'Pick git files' })
  vim.keymap.set('n', '<Space>pb', '<Cmd>Pick buffers<CR>', { desc = 'Pick buffers' })
  vim.keymap.set('n', '<Space>ph', '<Cmd>Pick help<CR>', { desc = 'Pick help' })
  vim.keymap.set('n', '<Space>pr', '<Cmd>Pick resume<CR>', { desc = 'Resume last pick' })
  vim.keymap.set('n', '<Space>p/', '<Cmd>Pick grep_live<CR>', { desc = 'Live grep' })
end
