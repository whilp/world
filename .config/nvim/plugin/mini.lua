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

  -- Function to get hostname or box-name/host_env
  local function get_host_identifier()
    -- Try to find and read /*/conf/box-name using vim.fn.glob
    local box_name_matches = vim.fn.glob('/*/conf/box-name', false, true)
    local box_name_path = box_name_matches[1]

    local identifier = ''
    if box_name_path and box_name_path ~= '' then
      local box_name_file = io.open(box_name_path, 'r')
      if box_name_file then
        identifier = box_name_file:read('*l') or ''
        box_name_file:close()
        identifier = identifier:gsub('\n', ''):gsub('^%s*(.-)%s*$', '%1')

        -- Try to append host_env
        if identifier ~= '' then
          local conf_dir = box_name_path:match('(.*)/box%-name$')
          if conf_dir then
            local host_env_file = io.open(conf_dir .. '/host_env', 'r')
            if host_env_file then
              local env = host_env_file:read('*l') or ''
              host_env_file:close()
              env = env:gsub('\n', ''):gsub('^%s*(.-)%s*$', '%1')
              if env ~= '' then
                identifier = identifier .. '/' .. env
              end
            end
          end
        end
      end
    end

    -- Fall back to short hostname
    if identifier == '' then
      local handle = io.popen('hostname -s')
      if handle then
        identifier = handle:read('*l') or ''
        handle:close()
        identifier = identifier:gsub('\n', ''):gsub('^%s*(.-)%s*$', '%1')
      end
    end

    return identifier
  end

  -- Function to convert string to deterministic hue (0-359)
  local function string_to_hue(str)
    local hash = 0
    for i = 1, #str do
      hash = (hash * 31 + string.byte(str, i)) % 2147483647
    end
    return hash % 360
  end

  -- Get host identifier and generate deterministic color scheme
  local host_id = get_host_identifier()
  local hue = string_to_hue(host_id)

  local base_colors = require("mini.hues").gen_random_base_colors({
    gen_hue = function() return hue end
  })

  require("mini.hues").setup({
    background = base_colors.background,
    foreground = base_colors.foreground,
  })

  -- mini.pick keybindings
  vim.keymap.set('n', '<Space>pf', '<Cmd>Pick files<CR>', { desc = 'Pick files' })
  vim.keymap.set('n', '<Space>pg', '<Cmd>Pick git_files<CR>', { desc = 'Pick git files' })
  vim.keymap.set('n', '<Space>pb', '<Cmd>Pick buffers<CR>', { desc = 'Pick buffers' })
  vim.keymap.set('n', '<Space>ph', '<Cmd>Pick help<CR>', { desc = 'Pick help' })
  vim.keymap.set('n', '<Space>pr', '<Cmd>Pick resume<CR>', { desc = 'Resume last pick' })
  vim.keymap.set('n', '<Space>p/', '<Cmd>Pick grep_live<CR>', { desc = 'Live grep' })
end
