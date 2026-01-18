-- luacheck ignore: neovim runtime
local map = vim.keymap.set

-- Terminal mode keybindings
map('t', [[<C-\>]], [[<C-\><C-n>]], { desc = 'Exit terminal mode' })
map({ 'n', 't' }, '<C-z>', '<C-z>', { desc = 'Pass Control-Z through' })
map({ 'n', 'i' }, [[<C-\>]], '<Esc>', { desc = 'Escape to normal mode' })

-- Terminal creation with environment setup
map('n', '<Space>te', function()
  local buf_id = vim.api.nvim_get_current_buf()
  local unique_id = 'nvim_' .. buf_id .. '_' .. os.time()

  vim.cmd('vsplit')
  vim.cmd('enew')  -- Create a new empty buffer

  -- Now call termopen in the clean buffer
  vim.b.terminal_job_id = vim.fn.termopen(vim.o.shell, {
    env = vim.tbl_extend('force', vim.fn.environ(), {
      NVIM_BUFFER_ID = unique_id,
      NVIM_SOCKET = vim.v.servername
    })
  })

  -- Store buffer ID mapping for this tab
  local tabnr = vim.fn.tabpagenr()
  vim.fn.settabvar(tabnr, 'buffer_id', unique_id)

  vim.cmd('startinsert')
end, { desc = 'Create terminal in vertical split' })

-- Terminal window navigation
map('t', '<D-j>', function()
  vim.cmd('stopinsert')
  -- Reference move_or_split_down from window.lua (will be available globally)
  if _G.move_or_split_down then
    _G.move_or_split_down()
  else
    vim.cmd('wincmd j')
  end
end, { desc = 'Move to window below or create split' })

map('t', '<D-k>', function()
  vim.cmd('stopinsert')
  -- Reference move_or_split_up from window.lua (will be available globally)
  if _G.move_or_split_up then
    _G.move_or_split_up()
  else
    vim.cmd('wincmd k')
  end
end, { desc = 'Move to window above or create split' })

map('t', '<D-h>', function()
  vim.cmd('stopinsert')
  -- Reference smart_move_left from window.lua (will be available globally)
  if _G.smart_move_left then
    _G.smart_move_left()
  else
    vim.cmd('wincmd h')
  end
end, { desc = 'Move to window left' })

map('t', '<D-l>', function()
  vim.cmd('stopinsert')
  -- Reference smart_move_right from window.lua (will be available globally)
  if _G.smart_move_right then
    _G.smart_move_right()
  else
    vim.cmd('wincmd l')
  end
end, { desc = 'Move to window right' })

-- Terminal buffer management
map('t', '<D-q>', '<C-\\><C-n><cmd>enew|bd #<cr>', { noremap = true, silent = true, desc = 'Close current terminal buffer' })

-- TODO: OSC passthrough causes terminal blocking in server mode
-- See: https://github.com/whilp/neovim/pull/4
-- vim.api.nvim_create_autocmd('TermRequest', {
--   group = vim.api.nvim_create_augroup('osc_passthrough', { clear = true }),
--   callback = function(args)
--     if #vim.api.nvim_list_uis() == 0 then return end
--     local seq = args.data.sequence
--     if seq and seq:match('^\027%]') then
--       pcall(vim.api.nvim_ui_send, seq)
--     end
--   end,
-- })
