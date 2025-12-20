local cosmo = require("cosmo")
local unix = cosmo.unix

-- Read entire file contents
-- Returns data, nil on success; nil, err on failure
local function read_file(path)
  local f, err = io.open(path, "rb")
  if not f then
    return nil, "failed to open: " .. (err or "unknown error")
  end
  local data = f:read("*a")
  f:close()
  return data
end

-- Atomic file copy with permissions
-- Creates file with restrictive perms, writes data, then sets final mode
-- Returns ok, err where ok is true on success, false on failure
local function copy_file(src, dst, mode, overwrite)
  -- Read source
  local src_f, err = io.open(src, "rb")
  if not src_f then
    return false, "failed to open source: " .. (err or "unknown error")
  end
  local data = src_f:read("*a")
  src_f:close()

  -- If overwriting and destination is a symlink, remove it first
  if overwrite then
    local st = unix.stat(dst, unix.AT_SYMLINK_NOFOLLOW)
    if st and unix.S_ISLNK(st.st_mode) then
      local unlink_ok = unix.unlink(dst)
      if not unlink_ok then
        return false, "failed to remove existing symlink"
      end
    end
  end

  -- Create or overwrite destination with restrictive permissions
  local flags = unix.O_WRONLY | unix.O_CREAT
  if overwrite then
    flags = flags | unix.O_TRUNC
  else
    flags = flags | unix.O_EXCL
  end

  local fd = unix.open(dst, flags, 0600)
  if not fd or fd < 0 then
    if overwrite then
      return false, "failed to open destination for writing"
    else
      return false, "destination already exists (use --force to overwrite)"
    end
  end

  -- Write data
  local bytes_written = unix.write(fd, data)
  local success = bytes_written == #data

  if not success then
    unix.close(fd)
    return false, "failed to write data (wrote " .. bytes_written .. " of " .. #data .. " bytes)"
  end

  -- Set final permissions if specified
  if mode then
    local chmod_ok = unix.chmod(dst, mode)
    if not chmod_ok then
      unix.close(fd)
      return false, "failed to set permissions"
    end
  end

  unix.close(fd)
  return true
end

-- Parse a single manifest line
-- Returns {path=string, mode=number|nil} or nil for skip (comment/empty)
local function parse_manifest_line(line)
  -- Skip comments and empty lines
  if line:match("^%s*#") or not line:match("%S") then
    return nil
  end

  -- Parse "filepath mode" format
  local file_path, mode_str = line:match("^(.-)%s+(%x+)$")
  if file_path and mode_str then
    local mode = tonumber(mode_str, 8)
    return { path = file_path, mode = mode }
  else
    -- Fallback for old format (no mode)
    return { path = line, mode = nil }
  end
end

-- Parse manifest content (string or line iterator)
-- Returns array of {path=string, mode=number|nil}
local function parse_manifest(input)
  local files = {}
  local lines
  if type(input) == "string" then
    lines = input:gmatch("[^\n]+")
  else
    lines = input
  end

  for line in lines do
    local entry = parse_manifest_line(line)
    if entry then
      table.insert(files, entry)
    end
  end
  return files
end

-- Extract tool names from manifest entries
-- Returns sorted array of tool names from .local/bin
local function extract_tools(files)
  local tools = {}
  for _, file_info in ipairs(files) do
    local tool = file_info.path:match("home/%.local/bin/([^/]+)$")
    if tool and not tools[tool] then
      tools[tool] = true
    end
  end

  local sorted = {}
  for tool in pairs(tools) do
    table.insert(sorted, tool)
  end
  table.sort(sorted)
  return sorted
end

-- Parse command-line arguments
-- Returns {cmd=string, force=bool, dest=string|nil}
local function parse_args(args)
  local result = {
    cmd = args[1] or "help",
    force = false,
    dest = nil,
  }

  local i = 2
  while i <= #args do
    if args[i] == "--force" or args[i] == "-f" then
      result.force = true
    elseif not result.dest then
      result.dest = args[i]
    end
    i = i + 1
  end

  return result
end

-- Strip "home/" prefix from path
local function strip_home_prefix(path)
  if path:sub(1, 5) == "home/" then
    return path:sub(6)
  end
  return path
end

-- Check if path is a directory (ends with /)
local function is_directory_path(path)
  return path:match("/$") ~= nil
end

local function cmd_unpack(dest, force, opts)
  opts = opts or {}
  local manifest_path = opts.manifest_path or "/zip/MANIFEST.txt"
  local zip_root = opts.zip_root or "/zip/"
  local stderr = opts.stderr or io.stderr

  if not dest then
    stderr:write("error: destination path required\n")
    stderr:write("usage: home unpack [--force] <destination>\n")
    return 1
  end

  stderr:write("extracting to " .. dest .. "...\n")
  if force then
    stderr:write("overwrite mode enabled\n")
  end

  -- Create destination directory
  if not unix.makedirs(dest) then
    stderr:write("error: failed to create destination directory\n")
    return 1
  end

  -- Read manifest to get list of files with modes
  local manifest = io.open(manifest_path, "r")
  if not manifest then
    stderr:write("error: failed to read manifest\n")
    return 1
  end

  local files = parse_manifest(manifest:lines())
  manifest:close()

  -- Copy each file from zip to destination
  for _, file_info in ipairs(files) do
    local file_path = file_info.path
    local mode = file_info.mode
    local rel_path = strip_home_prefix(file_path)
    local zip_file_path = zip_root .. file_path
    local dest_file_path = dest .. "/" .. rel_path

    if not is_directory_path(file_path) then
      -- Create parent directory
      local parent_dir = cosmo.path.dirname(dest_file_path)
      unix.makedirs(parent_dir)

      -- Copy file atomically with permissions
      local ok, err = copy_file(zip_file_path, dest_file_path, mode, force)
      if not ok then
        stderr:write("warning: failed to copy " .. file_path .. ": " .. (err or "unknown error") .. "\n")
      end
    else
      -- Create directory
      unix.makedirs(dest .. "/" .. rel_path)
    end
  end

  stderr:write("extraction complete\n")
  stderr:write("add " .. dest .. "/.local/bin to PATH if needed\n")
  return 0
end

local function cmd_list(opts)
  opts = opts or {}
  local manifest_path = opts.manifest_path or "/zip/MANIFEST.txt"
  local stdout = opts.stdout or io.stdout
  local stderr = opts.stderr or io.stderr

  -- Read manifest to get list of files
  local manifest = io.open(manifest_path, "r")
  if not manifest then
    stderr:write("error: failed to read manifest\n")
    return 1
  end

  local files = parse_manifest(manifest:lines())
  manifest:close()

  local tools = extract_tools(files)

  stdout:write("embedded files: " .. #files .. " total\n")
  stdout:write("  - dotfiles (~/.zshrc, ~/.config/*, etc.)\n")
  stdout:write("  - binaries (~/.local/bin/*)\n")
  stdout:write("\nembedded tools:\n")

  for _, tool in ipairs(tools) do
    stdout:write("  - " .. tool .. "\n")
  end

  return 0
end

local function cmd_version(opts)
  opts = opts or {}
  local stdout = opts.stdout or io.stdout
  stdout:write("home built COMMIT_PLACEHOLDER\n")
  return 0
end

local function cmd_help(opts)
  opts = opts or {}
  local stderr = opts.stderr or io.stderr
  stderr:write("usage: home <command> [args]\n")
  stderr:write("\ncommands:\n")
  stderr:write("  unpack [--force] <dest>  - extract dotfiles and binaries to destination\n")
  stderr:write("  list                     - list embedded tools\n")
  stderr:write("  version                  - show build version\n")
  stderr:write("\noptions:\n")
  stderr:write("  --force, -f              - overwrite existing files\n")
  return 0
end

local function main(args, opts)
  opts = opts or {}
  local parsed = parse_args(args)

  if parsed.cmd == "unpack" then
    return cmd_unpack(parsed.dest, parsed.force, opts)
  elseif parsed.cmd == "list" then
    return cmd_list(opts)
  elseif parsed.cmd == "version" then
    return cmd_version(opts)
  elseif parsed.cmd == "help" then
    return cmd_help(opts)
  else
    cmd_help(opts)
    return 1
  end
end

local home = {
  read_file = read_file,
  copy_file = copy_file,
  parse_manifest_line = parse_manifest_line,
  parse_manifest = parse_manifest,
  extract_tools = extract_tools,
  parse_args = parse_args,
  strip_home_prefix = strip_home_prefix,
  is_directory_path = is_directory_path,
  cmd_unpack = cmd_unpack,
  cmd_list = cmd_list,
  cmd_version = cmd_version,
  cmd_help = cmd_help,
  main = main,
}

-- Run main if executed directly (not required as module)
if arg and arg[0] and arg[0]:match("/main%.lua$") then
  os.exit(main({ ... }) or 0)
end

return home
