local ok, cosmo = pcall(require, 'cosmo')
local unix = ok and cosmo.unix or nil
local spawn_ok, spawn_mod = pcall(require, 'spawn')
local spawn = spawn_ok and spawn_mod.spawn or nil

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
  if not unix then
    local f = io.open(path, 'r')
    if f then
      f:close()
      return true
    end
    return false
  end
  return unix.access(path, unix.F_OK)
end

local cached_conf_dir = nil

local function find_conf_dir()
  if cached_conf_dir ~= nil then
    return cached_conf_dir
  end

  if not unix then
    cached_conf_dir = false
    return nil
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
  local hostname
  if spawn then
    local s_ok, output = spawn({'hostname', '-s'}):read()
    if s_ok and output then
      hostname = output
    else
      s_ok, output = spawn({'hostname'}):read()
      if s_ok and output then
        hostname = output
      end
    end
  else
    local handle = io.popen('hostname -s 2>/dev/null || hostname', 'r')
    if handle then
      hostname = handle:read('*l')
      handle:close()
    end
  end
  if hostname then
    return trim(hostname):match('([^.%s]+)')
  end
  return nil
end

local function get_git_branch()
  local branch
  if spawn then
    local s_ok, output = spawn({'git', 'rev-parse', '--abbrev-ref', 'HEAD'}):read()
    if s_ok and output then
      branch = trim(output)
    end
  else
    local handle = io.popen('git rev-parse --abbrev-ref HEAD 2>/dev/null', 'r')
    if handle then
      branch = handle:read('*l')
      handle:close()
      if branch then
        branch = trim(branch)
      end
    end
  end
  return branch
end

local function is_codespace()
  return os.getenv('CODESPACES') == 'true'
end

local function get_repo_name()
  local repo = os.getenv('GITHUB_REPOSITORY')
  if repo then
    return repo:match('[^/]+/(.+)') or repo
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

      local env = read_file(conf_dir .. '/host_env')
      if env and env ~= '' then
        identifier = identifier .. '.' .. env
      end
    end
  end

  if identifier == '' then
    identifier = get_short_hostname() or 'unknown'
  end

  return identifier
end

local function get_with_emoji()
  local identifier = get()
  local emoji = ''

  local conf_dir = find_conf_dir()
  if conf_dir then
    emoji = read_file(conf_dir .. '/box-emoji')
  end

  if not emoji or emoji == '' then
    emoji = string_to_emoji(identifier)
  end

  if is_codespace() then
    local repo = get_repo_name()
    local branch = get_git_branch()
    if repo and branch then
      local hostname = get_short_hostname() or identifier
      return repo .. '/' .. branch .. ' | ' .. hostname .. ' ' .. emoji
    end
  end

  return identifier .. ' ' .. emoji
end

return {
  get = get,
  get_with_emoji = get_with_emoji,
}
