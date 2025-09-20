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
end
