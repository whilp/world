vim.pack.add({
  { src = "https://github.com/nvim-mini/mini.nvim" },
})

local ok, mini = pcall(require, "mini")
if ok then
  -- Initialize mini.nvim modules as needed
  -- Example: mini.statusline.setup()
  -- Example: mini.tabline.setup()
  -- Example: mini.pairs.setup()
  require("mini.bufremove").setup()
  require("mini.diff").setup()
  require("mini.git").setup()
  require("mini.pick").setup()
end
