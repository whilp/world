-- luacheck ignore: neovim runtime
--check:false
local opt = vim.opt

-- File handling
opt.autoread = true
opt.hidden = true

-- Shell configuration
local zsh_paths = { "/usr/bin/zsh", "/bin/zsh" }
for _, path in ipairs(zsh_paths) do
  if vim.fn.executable(path) == 1 then
    vim.o.shell = path
    break
  end
end

vim.env.NVIM_INVIM = "true"

-- OSC52 clipboard configuration
vim.g.clipboard = "osc52"
