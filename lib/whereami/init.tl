local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local function trim(s: string): string
  return s:match('^%s*(.-)%s*$')
end

local function read_file(filepath: string): string
  local content = cosmo.Slurp(filepath)
  if not content then
    return nil
  end
  local first_line = content:match('^[^\n]*')
  return first_line and trim(first_line) or nil
end

local function file_exists(path: string): boolean
  return unix.access(path, unix.F_OK)
end

-- can be nil (not yet cached), false (cached as not found), or string (cached path)
local cached_conf_dir: boolean | string = nil

local function find_conf_dir(): string
  if cached_conf_dir ~= nil then
    if cached_conf_dir is string then
      return cached_conf_dir
    end
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

local function string_to_emoji(str: string): string
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

local function get_short_hostname(): string
  local hostname = unix.gethostname()
  if hostname then
    return hostname:match('([^.%s]+)')
  end
  return nil
end

local function get_box_dir(): string
  local home = os.getenv("HOME")
  if not home then
    return nil
  end
  return path.join(home, ".config", "box")
end

local function get_box_name(): string
  local box_dir = get_box_dir()
  if not box_dir then
    return nil
  end
  return read_file(path.join(box_dir, "name"))
end

local function get_box_kind(): string
  local box_dir = get_box_dir()
  if not box_dir then
    return nil
  end
  return read_file(path.join(box_dir, "kind"))
end

local function get(): string
  local identifier = ''

  -- Check for box name and kind first (stored during box bootstrap)
  local box_name = get_box_name()
  local box_kind = get_box_kind()
  if box_name and box_name ~= '' and box_kind and box_kind ~= '' then
    return box_name .. "." .. box_kind
  end

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

local type EnvFunc = function(string): string

local function get_with_emoji(env: EnvFunc): string
  env = env or os.getenv
  local identifier = get()
  local emoji: string = ''

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
