-- Module to get hostname or box-name/host_env identifier
local M = {}

local ffi = require('ffi')

ffi.cdef[[
  typedef struct DIR DIR;
  typedef struct dirent {
    uint64_t d_ino;
    int64_t d_off;
    unsigned short d_reclen;
    unsigned char d_type;
    char d_name[256];
  } dirent;

  DIR *opendir(const char *name);
  dirent *readdir(DIR *dirp);
  int closedir(DIR *dirp);
  int access(const char *pathname, int mode);
]]

local C = ffi.C
local F_OK = 0

-- Function to trim whitespace
local function trim(s)
  return s:match('^%s*(.-)%s*$')
end

-- Function to read file contents
local function read_file(path)
  local file = io.open(path, 'r')
  if not file then
    return nil
  end
  local content = file:read('*l')
  file:close()
  return content and trim(content) or nil
end

-- Function to check if file exists
local function file_exists(path)
  return C.access(path, F_OK) == 0
end

-- Cache for conf directory path
local cached_conf_dir = nil

-- Function to find the conf directory (scans root once and caches result)
local function find_conf_dir()
  if cached_conf_dir ~= nil then
    return cached_conf_dir
  end

  local dir = C.opendir('/')
  if dir == nil then
    return nil
  end

  while true do
    local entry = C.readdir(dir)
    if entry == nil then
      break
    end

    local name = ffi.string(entry.d_name)
    if name ~= '.' and name ~= '..' then
      local conf_path = '/' .. name .. '/conf'
      -- Check if conf directory exists by checking for box-name file
      if file_exists(conf_path .. '/box-name') then
        C.closedir(dir)
        cached_conf_dir = conf_path
        return conf_path
      end
    end
  end

  C.closedir(dir)
  cached_conf_dir = false  -- Cache negative result
  return nil
end

-- Function to deterministically pick an emoji based on a string
local function string_to_emoji(str)
  -- Food & Drink emojis from https://emojipedia.org/food-drink/ (color-blind friendly)
  -- Must be no newer than Unicode 9.0 to prevent Terminal issues
  -- From pay-server/dev/lib/devbox/emojier/translator.rb
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

  -- Generate hash from string
  local hash = 0
  for i = 1, #str do
    hash = (hash * 31 + string.byte(str, i)) % 2147483647
  end

  -- Pick emoji based on hash
  local index = (hash % #emojis) + 1
  return emojis[index]
end

-- Main function to get the identifier
function M.get()
  local identifier = ''

  -- Try to find conf directory and read box-name
  local conf_dir = find_conf_dir()
  if conf_dir then
    local box_name = read_file(conf_dir .. '/box-name')
    if box_name and box_name ~= '' then
      identifier = box_name

      -- Try to append host_env
      local env = read_file(conf_dir .. '/host_env')
      if env and env ~= '' then
        identifier = identifier .. '.' .. env
      end
    end
  end

  -- Fall back to short hostname
  if identifier == '' then
    local unistd = require('posix.unistd')
    local wait = require('posix.sys.wait')
    local posix = require('posix')
    local handle = posix.popen({'/bin/sh', '-c', 'hostname -s 2>/dev/null || hostname'}, 'r')
    if handle then
      local hostname = unistd.read(handle.fd, 1024)
      wait.wait(handle.pids[1])
      unistd.close(handle.fd)
      if hostname then
        identifier = hostname:match('([^.%s]+)')
      end
    end
  end

  return identifier ~= '' and identifier or 'unknown'
end

-- Function to get identifier with emoji suffix
function M.get_with_emoji()
  local identifier = M.get()
  local emoji = ''

  -- Try to read /*/conf/box-emoji using cached conf directory
  local conf_dir = find_conf_dir()
  if conf_dir then
    emoji = read_file(conf_dir .. '/box-emoji')
  end

  -- Fall back to deterministic emoji if not found
  if not emoji or emoji == '' then
    emoji = string_to_emoji(identifier)
  end

  return identifier .. ' ' .. emoji
end

return M
