-- Module to get hostname or box-name/host_env identifier
local M = {}

-- Function to get hostname or box-name/host_env
function M.get()
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

return M
