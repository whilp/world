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

-- OSC passthrough to parent terminal for notifications
-- Handles OSC 9 and OSC 777 sequences from applications running in :terminal
vim.api.nvim_create_autocmd("TermRequest", {
  group = vim.api.nvim_create_augroup("osc_passthrough", { clear = true }),
  callback = function(args)
    local payload = args.data and args.data.payload
    if not payload then return end

    -- Check for OSC 9 (notification) or OSC 777 (notify) sequences
    -- OSC sequences start with ESC ] and end with BEL or ESC \
    if payload:match("^9;") or payload:match("^777;notify;") then
      -- Write the full OSC sequence to stdout to pass to parent terminal
      -- ESC ] is \x1b], BEL is \x07
      io.stdout:write(string.format("\x1b]%s\x07", payload))
      io.stdout:flush()
    end
  end,
})
