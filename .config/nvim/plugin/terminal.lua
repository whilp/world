local map = vim.keymap.set

-- Terminal mode keybindings
map('t', [[<C-\>]], [[<C-\><C-n>]])
map({ 'n', 't' }, '<C-z>', '<C-z>', { desc = 'Pass Control-Z through' })
map({ 'n', 'i' }, [[<C-\>]], '<Esc>')

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
      NVIM_SOCKET = '~/.config/nvim/nvim.sock'
    })
  })
  
  -- Store buffer ID mapping for this tab
  local tabnr = vim.fn.tabpagenr()
  vim.fn.settabvar(tabnr, 'buffer_id', unique_id)
  
  vim.cmd('startinsert')
end)

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

map('t', '<D-h>', '<C-\\><C-n><C-w>h', { desc = 'Move to window left' })
map('t', '<D-l>', '<C-\\><C-n><C-w>l', { desc = 'Move to window right' })

-- Terminal buffer management
map('t', '<D-q>', '<C-\\><C-n><cmd>enew|bd #<cr>', { noremap = true, silent = true })