local unix = require("cosmo.unix")
local spawn = require('spawn').spawn

local function trim(s)
  return s:match('^%s*(.-)%s*$')
end

local function read_file(path)
  local file = io.open(path, 'r')
  if not file then
    return nil
  end
  local content = file:read('*l')
  file:close()
  return content and trim(content) or nil
end

local function file_exists(path)
  return unix.access(path, unix.F_OK)
end

local cached_conf_dir = nil

local function find_conf_dir()
  if cached_conf_dir ~= nil then
    return cached_conf_dir
  end

  local dir = unix.opendir('/')
  if not dir then
    return nil
  end

  while true do
    local name = dir:read()
    if not name then
      break
    end

    if name ~= '.' and name ~= '..' then
      local conf_path = '/' .. name .. '/conf'
      if file_exists(conf_path .. '/box-name') then
        dir:close()
        cached_conf_dir = conf_path
        return conf_path
      end
    end
  end

  dir:close()
  cached_conf_dir = false
  return nil
end

local function string_to_emoji(str)
  local emojis = {
    '🍇', '🍈', '🍉', '🍊', '🍋', '🍌', '🍍', '🍎', '🍐', '🍑',
    '🍒', '🍓', '🥝', '🍅', '🥑', '🍆', '🥔', '🥕', '🌽', '🌶',
    '🥒', '🍄', '🥜', '🌰', '🍞', '🥐', '🥖', '🥞', '🧀', '🍖',
    '🍗', '🥓', '🍔', '🍟', '🍕', '🌭', '🌮', '🌯', '🥙', '🥚',
    '🥗', '🍿', '🍱', '🍘', '🍙', '🍛', '🍠', '🍢', '🍣', '🍤',
    '🍥', '🍡', '🍦', '🍧', '🍨', '🍩', '🍪', '🎂', '🍰', '🍫',
    '🍬', '🍭', '🍮', '🍯', '🍼', '🥛', '🍵', '🍶', '🍷', '🍸',
    '🍹', '🍺', '🍻', '🥂', '🥃'
  }

  local hash = 0
  for i = 1, #str do
    hash = (hash * 31 + string.byte(str, i)) % 2147483647
  end

  local index = (hash % #emojis) + 1
  return emojis[index]
end

local function get_short_hostname()
  local ok, output = spawn({'hostname', '-s'}):read()
  if ok and output then
    return trim(output):match('([^.%s]+)')
  end
  ok, output = spawn({'hostname'}):read()
  if ok and output then
    return trim(output):match('([^.%s]+)')
  end
  return nil
end

local function get()
  local identifier = ''

  local conf_dir = find_conf_dir()
  if conf_dir then
    local box_name = read_file(conf_dir .. '/box-name')
    if box_name and box_name ~= '' then
      identifier = box_name

      local host_env = read_file(conf_dir .. '/host_env')
      if host_env and host_env ~= '' then
        identifier = identifier .. '.' .. host_env
      end
    end
  end

  if identifier == '' then
    identifier = get_short_hostname() or 'unknown'
  end

  return identifier
end

local function get_with_emoji(env)
  env = env or os.getenv
  local identifier = get()
  local emoji = ''

  local conf_dir = find_conf_dir()
  if conf_dir then
    emoji = read_file(conf_dir .. '/box-emoji')
  end

  if not emoji or emoji == '' then
    emoji = string_to_emoji(identifier)
  end

  if env('CODESPACES') == 'true' then
    local repo_full = env('GITHUB_REPOSITORY')
    local repo = repo_full and (repo_full:match('[^/]+/(.+)') or repo_full)
    if repo then
      local codespace_name = env('CODESPACE_NAME') or identifier
      codespace_name = codespace_name:match('(.+)-[^-]+$') or codespace_name
      return repo .. ' | ' .. codespace_name .. ' ' .. emoji
    end
  end

  return identifier .. ' ' .. emoji
end

return {
  get = get,
  get_with_emoji = get_with_emoji,
}
