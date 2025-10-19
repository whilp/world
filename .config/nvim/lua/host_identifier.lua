-- Module to get hostname or box-name/host_env identifier
local M = {}

-- Function to get hostname or box-name/host_env
function M.get()
  local handle = io.popen('whereami 2>/dev/null')
  if not handle then
    return 'unknown'
  end

  local identifier = handle:read('*l') or ''
  handle:close()

  return identifier ~= '' and identifier or 'unknown'
end

return M
