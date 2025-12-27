local map = vim.keymap.set

-- Tab management
map('n', '<Space>tt', ':tabnew<CR>', { desc = 'Create new tab' })

-- Tab switching with CMD-1 through CMD-9
for i = 1, 9 do
  map('n', '<D-' .. i .. '>', i .. 'gt', { desc = 'Switch to tab ' .. i })
  map('i', '<D-' .. i .. '>', '<C-o>' .. i .. 'gt', { desc = 'Switch to tab ' .. i })
  map('t', '<D-' .. i .. '>', '<C-\\><C-n>' .. i .. 'gt', { desc = 'Switch to tab ' .. i })
end

-- Tab switching with CMD-Shift-[ and CMD-Shift-]
map('n', '<S-D-[>', 'gT', { desc = 'Switch to previous tab' })
map('i', '<S-D-[>', '<C-o>gT', { desc = 'Switch to previous tab' })
map('t', '<S-D-[>', '<C-\\><C-n>gT', { desc = 'Switch to previous tab' })

map('n', '<S-D-]>', 'gt', { desc = 'Switch to next tab' })
map('i', '<S-D-]>', '<C-o>gt', { desc = 'Switch to next tab' })
map('t', '<S-D-]>', '<C-\\><C-n>gt', { desc = 'Switch to next tab' })

-- Create new tab with directory with CMD-T
map('n', '<D-t>', function()
  -- Get current tab's local directory, fallback to global cwd
  local current_dir = vim.fn.exists('*haslocaldir') == 1 and vim.fn.haslocaldir() == 1
    and vim.fn.getcwd() or vim.fn.getcwd()

  vim.ui.input({
    prompt = 'Directory path: ',
    default = current_dir,
    completion = 'dir'
  }, function(input)
    if input ~= nil then
      _G.create_tab_with_directory(input)
    end
  end)
end, { desc = 'Create tab with directory' })
map('i', '<D-t>', function()
  -- Get current tab's local directory, fallback to global cwd
  local current_dir = vim.fn.exists('*haslocaldir') == 1 and vim.fn.haslocaldir() == 1
    and vim.fn.getcwd() or vim.fn.getcwd()

  vim.ui.input({
    prompt = 'Directory path: ',
    default = current_dir,
    completion = 'dir'
  }, function(input)
    if input ~= nil then
      _G.create_tab_with_directory(input)
    end
  end)
end, { desc = 'Create tab with directory' })
map('t', '<D-t>', function()
  -- Get current tab's local directory, fallback to global cwd
  local current_dir = vim.fn.exists('*haslocaldir') == 1 and vim.fn.haslocaldir() == 1
    and vim.fn.getcwd() or vim.fn.getcwd()

  vim.ui.input({
    prompt = 'Directory path: ',
    default = current_dir,
    completion = 'dir'
  }, function(input)
    if input ~= nil then
      _G.create_tab_with_directory(input)
    end
  end)
end, { desc = 'Create tab with directory' })

-- Close current tab with CMD-W
map('n', '<D-w>', ':tabclose<CR>', { desc = 'Close current tab' })
map('i', '<D-w>', '<C-o>:tabclose<CR>', { desc = 'Close current tab' })
map('t', '<D-w>', '<C-\\><C-n>:tabclose<CR>', { desc = 'Close current tab' })

-- Function to generate shortened directory name
local function generate_short_dir_name(dir_path)
  -- Remove trailing slashes
  local clean_path = dir_path:gsub('/+$', '')
  local home_path = vim.fn.fnamemodify(clean_path, ':~')
  local parts = vim.split(home_path, '/')

  if #parts <= 2 then
    return home_path
  else
    local shortened_parts = {}
    -- Shorten all parts except the last one
    for i = 1, #parts - 1 do
      if parts[i] ~= '' then
        table.insert(shortened_parts, parts[i]:sub(1, 1))
      else
        table.insert(shortened_parts, parts[i])
      end
    end
    -- Keep the last directory name full
    table.insert(shortened_parts, parts[#parts])
    return table.concat(shortened_parts, '/')
  end
end

-- Function to create a tab with directory
_G.create_tab_with_directory = function(dir_path)
  if not dir_path or dir_path == '' then
    print('Error: Directory path required')
    return
  end

  -- Expand path
  local expanded_path = vim.fn.expand(dir_path)

  -- Check if directory exists
  if vim.fn.isdirectory(expanded_path) ~= 1 then
    print('Error: Directory does not exist: ' .. expanded_path)
    return
  end

  -- Create new tab
  vim.cmd('tabnew')

  -- Set tab-local directory
  vim.cmd('tcd ' .. vim.fn.fnameescape(expanded_path))

  -- Generate shortened path name
  local short_name = generate_short_dir_name(expanded_path)

  -- Set manual tab name to prevent auto-naming
  vim.fn.settabvar(vim.fn.tabpagenr(), 'tab_name', short_name)
  vim.cmd('redrawtabline')
end

-- Tab naming keybindings
map('n', '<Space>tr', function() _G.force_update_all_tab_names() end, { desc = 'Refresh all tab names' })
map('n', '<Space>tc', function() _G.force_update_current_tab_name() end, { desc = 'Refresh current tab name' })
map('n', '<Space>tn', function()
  vim.ui.input({ prompt = 'Tab name (empty to auto-name): ' }, function(input)
    if input ~= nil then
      vim.cmd('TabName ' .. input)
    end
  end)
end, { desc = 'Set manual tab name' })

-- Configure tabline to show tab titles
_G.render_tabline = function()
  local s = ' '  -- left margin
  for i = 1, vim.fn.tabpagenr('$') do
    -- Use manual tab names or default to "Tab N"
    local title = vim.fn.gettabvar(i, 'tab_name', 'Tab ' .. i)

    -- Highlight current tab
    if i == vim.fn.tabpagenr() then
      s = s .. '%#TabLineSel#'
    else
      s = s .. '%#TabLine#'
    end

    -- Format tab number with filled triangle for active tab, outline triangle for inactive
    local tab_number = (i == vim.fn.tabpagenr()) and '▶ ' .. i .. ' ' .. title or '▷ ' .. i .. ' ' .. title
    s = s .. '%' .. i .. 'T' .. ' ' .. tab_number .. ' '
  end

  -- Add right-aligned section with host identifier
  s = s .. '%#TabLineFill#%T'

  local whereami = os.getenv('WHEREAMI') or ''
  if whereami ~= '' then
    s = s .. '%=' .. '%#TabLine# ' .. whereami .. ' '
  else
    s = s .. '%=%#TabLineFill# '
  end

  return s
end

vim.opt.tabline = [[%!v:lua.render_tabline()]]


-- Enhanced tab naming with buffer content analysis
tab_namer = {
  pending_requests = {}, -- Track pending requests per tab
  last_update_time = {}, -- Track last update time per tab
  debounce_ms = 5000     -- 5 second debounce
}

-- Function to collect buffer information for a tab
local function collect_tab_data(tabnr)
  local tab_data = {
    files = {},
    content = "",
    has_terminal = false
  }

  -- Get all buffers in this tab
  local tab_buffers = vim.fn.tabpagebuflist(tabnr)
  local content_parts = {}

  for _, bufnr in ipairs(tab_buffers) do
    local bufname = vim.api.nvim_buf_get_name(bufnr)
    local buftype = vim.api.nvim_buf_get_option(bufnr, 'buftype')

    if buftype == 'terminal' then
      tab_data.has_terminal = true
      -- Get terminal content (last 20 lines)
      local lines = vim.api.nvim_buf_get_lines(bufnr, -20, -1, false)
      local terminal_content = table.concat(lines, ' ')
      if terminal_content and #terminal_content > 0 then
        table.insert(content_parts, terminal_content:sub(1, 200))
      end
    elseif bufname and bufname ~= "" and buftype == "" then
      -- Regular file buffer
      table.insert(tab_data.files, bufname)

      -- Get content sample (first 30 lines)
      local ok, lines = pcall(vim.api.nvim_buf_get_lines, bufnr, 0, 30, false)
      if ok and lines then
        local file_content = table.concat(lines, ' ')
        if file_content and #file_content > 0 then
          table.insert(content_parts, file_content:sub(1, 300))
        end
      end
    end
  end

  -- Combine all content
  tab_data.content = table.concat(content_parts, ' | '):sub(1, 500)

  return tab_data
end

-- Function to generate title for a single tab
local function generate_single_tab_title(tabnr)
  -- Skip auto-naming if user has set a manual name
  local manual_name = vim.fn.gettabvar(tabnr, 'tab_name', '')
  if manual_name ~= '' then
    return
  end

  -- Check if request is already pending for this tab
  if tab_namer.pending_requests[tabnr] then
    return
  end

  -- Check debounce timing
  local current_time = vim.loop.now()
  local last_time = tab_namer.last_update_time[tabnr] or 0
  if current_time - last_time < tab_namer.debounce_ms then
    return
  end

  -- Mark request as pending
  tab_namer.pending_requests[tabnr] = true
  tab_namer.last_update_time[tabnr] = current_time

  local tab_data = collect_tab_data(tabnr)

  -- Convert to JSON for the external script
  local json_data = vim.json.encode(tab_data)

  -- Call the external claude-tab-namer script
  local cmd = {'bash', '-c', 'echo ' .. vim.fn.shellescape(json_data) .. ' | ~/.local/bin/claude-tab-namer'}

  vim.system(cmd, {timeout = 20000}, function(result)
    local title = "tab"

    if result.code == 0 and result.stdout then
      local generated_title = result.stdout:gsub('\n', ''):gsub('^%s*(.-)%s*$', '%1')
      if generated_title and #generated_title > 0 and #generated_title <= 25 then
        title = generated_title
      end
    end

    vim.schedule(function()
      -- Clear pending request
      tab_namer.pending_requests[tabnr] = nil

      vim.fn.settabvar(tabnr, 'claude_title', title)
      vim.cmd('redrawtabline')
    end)
  end)
end

-- Function to force update a single tab (bypass debounce)
local function force_update_tab_title(tabnr)
  -- Skip auto-naming if user has set a manual name
  local manual_name = vim.fn.gettabvar(tabnr, 'tab_name', '')
  if manual_name ~= '' then
    return
  end

  -- Cancel any pending request
  tab_namer.pending_requests[tabnr] = nil

  -- Force update by resetting timing
  tab_namer.last_update_time[tabnr] = 0

  generate_single_tab_title(tabnr)
end

-- Function to update all tab titles
_G.update_all_tab_names = function()
  for i = 1, vim.fn.tabpagenr('$') do
    generate_single_tab_title(i)
  end
end

-- Function to update current tab title
_G.update_current_tab_name = function()
  generate_single_tab_title(vim.fn.tabpagenr())
end

-- Function to force update all tab titles (bypass debounce)
_G.force_update_all_tab_names = function()
  for i = 1, vim.fn.tabpagenr('$') do
    force_update_tab_title(i)
  end
end

-- Function to force update current tab title (bypass debounce)
_G.force_update_current_tab_name = function()
  force_update_tab_title(vim.fn.tabpagenr())
end

-- User commands for tab naming
vim.api.nvim_create_user_command('TabNameUpdate', function()
  _G.force_update_all_tab_names()
end, { desc = 'Update all tab names using Claude' })

vim.api.nvim_create_user_command('TabNameCurrent', function()
  _G.force_update_current_tab_name()
end, { desc = 'Update current tab name using Claude' })

vim.api.nvim_create_user_command('TabName', function(opts)
  local name = opts.args
  local tabnr = vim.fn.tabpagenr()

  if name == '' then
    -- Clear manual name
    vim.fn.settabvar(tabnr, 'tab_name', '')
    vim.cmd('redrawtabline')
  else
    -- Set manual name
    vim.fn.settabvar(tabnr, 'tab_name', name)
    vim.cmd('redrawtabline')
  end
end, {
  nargs = '?',
  desc = 'Set manual tab name (empty to clear)'
})

-- Auto-commands for tab management

-- Set initial tab title when creating new tabs
vim.api.nvim_create_autocmd('TabNew', {
  callback = function()
    vim.schedule(function()
      vim.cmd('redrawtabline')
    end)
  end
})

-- Auto-update tab names on certain events (disabled)
-- vim.api.nvim_create_autocmd({'TabNew', 'BufEnter', 'BufWritePost'}, {
--   callback = function()
--     vim.schedule(function()
--       _G.update_current_tab_name()
--     end)
--   end,
--   desc = 'Auto-update tab names on buffer/tab events'
-- })

-- Clean up tracking data for closed tabs
local function cleanup_closed_tabs()
  local existing_tabs = {}
  for i = 1, vim.fn.tabpagenr('$') do
    existing_tabs[i] = true
  end

  -- Clean up tracking for non-existent tabs
  for tabnr, _ in pairs(tab_namer.pending_requests) do
    if not existing_tabs[tabnr] then
      tab_namer.pending_requests[tabnr] = nil
      tab_namer.last_update_time[tabnr] = nil
    end
  end
end

-- Clean up on tab close
vim.api.nvim_create_autocmd('TabClosed', {
  callback = function()
    vim.schedule(cleanup_closed_tabs)
  end,
  desc = 'Clean up tab naming tracking data'
})

-- Set up first tab on startup
vim.api.nvim_create_autocmd('VimEnter', {
  callback = function()
    -- Only set up first tab if no tabs have manual names yet
    local first_tab_name = vim.fn.gettabvar(1, 'tab_name', '')
    if first_tab_name == '' then
      local cwd = vim.fn.getcwd()
      local short_name = generate_short_dir_name(cwd)
      vim.fn.settabvar(1, 'tab_name', short_name)
      vim.cmd('redrawtabline')
    end
  end,
  desc = 'Set up first tab name on startup'
})

-- Timer-based refresh (disabled)
-- local refresh_timer = vim.loop.new_timer()
-- refresh_timer:start(1800000, 1800000, vim.schedule_wrap(function()
--   cleanup_closed_tabs()
--   _G.update_all_tab_names()
-- end))
