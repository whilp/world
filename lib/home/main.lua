local cosmo = require("cosmo")
local unix = cosmo.unix
local path = cosmo.path

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
    if st and unix.S_ISLNK(st:mode()) then
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
-- Returns {cmd, subcmd, force, verbose, dry_run, only, null, dest}
local function parse_args(args)
  local result = {
    cmd = args[1] or "help",
    subcmd = nil,
    force = false,
    verbose = false,
    dry_run = false,
    only = false,
    null = false,
    dest = nil,
  }

  local i = 2
  while i <= #args do
    if args[i] == "--force" or args[i] == "-f" then
      result.force = true
    elseif args[i] == "--verbose" or args[i] == "-v" then
      result.verbose = true
    elseif args[i] == "--dry-run" or args[i] == "-n" then
      result.dry_run = true
    elseif args[i] == "--only" then
      result.only = true
    elseif args[i] == "--null" or args[i] == "-0" then
      result.null = true
    elseif result.cmd == "3p" and not result.subcmd and not args[i]:match("^%-") then
      result.subcmd = args[i]
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

-- Convert octal mode to permission string (e.g., 0644 -> "-rw-r--r--")
-- Returns 10-character string: type + owner + group + other permissions
local function format_mode(mode, is_dir)
  if not mode then
    return "----------"
  end

  local result = {}

  -- File type
  table.insert(result, is_dir and "d" or "-")

  -- Owner, group, other permissions (3 bits each)
  for i = 2, 0, -1 do
    local shift = i * 3
    local perms = (mode >> shift) & 7
    table.insert(result, (perms & 4) ~= 0 and "r" or "-")
    table.insert(result, (perms & 2) ~= 0 and "w" or "-")
    table.insert(result, (perms & 1) ~= 0 and "x" or "-")
  end

  return table.concat(result)
end

local function cmd_unpack(dest, force, opts)
  opts = opts or {}
  local manifest_path = opts.manifest_path or "/zip/MANIFEST.txt"
  local zip_root = opts.zip_root or "/zip/"
  local stderr = opts.stderr or io.stderr
  local stdout = opts.stdout or io.stdout
  local verbose = opts.verbose or false
  local dry_run = opts.dry_run or false
  local only = opts.only or false

  if not dest then
    stderr:write("error: destination path required\n")
    stderr:write("usage: home unpack [--force] <destination>\n")
    return 1
  end

  -- Read filter from stdin if --only is specified
  local filter = nil
  if only then
    filter = {}
    local input = opts.filter_input or io.stdin:read("*a")
    local delimiter = opts.null and string.char(0) or "\n"
    local pattern = opts.null and "[^\0]+" or "[^\n]+"

    for line in input:gmatch(pattern) do
      local path = line:match("^%s*(.-)%s*$")
      if path and path ~= "" then
        filter[path] = true
      end
    end
  end

  -- Create destination directory
  if not dry_run then
    if not unix.makedirs(dest) then
      stderr:write("error: failed to create destination directory\n")
      return 1
    end
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
    local dest_file_path = path.join(dest, rel_path)

    -- Skip if filter is active and path not in filter
    if filter and not filter[rel_path] then
      goto continue
    end

    if not is_directory_path(file_path) then
      -- Check if file exists before copying (for verbose overwrite detection)
      local file_exists = not dry_run and unix.stat(dest_file_path) ~= nil

      if not dry_run then
        -- Create parent directory
        local parent_dir = path.dirname(dest_file_path)
        unix.makedirs(parent_dir)

        -- Copy file atomically with permissions
        local ok, err = copy_file(zip_file_path, dest_file_path, mode, force)
        if not ok then
          stderr:write("warning: failed to copy " .. file_path .. ": " .. (err or "unknown error") .. "\n")
        elseif verbose then
          if force and file_exists then
            stdout:write(rel_path .. " (overwritten)\n")
          else
            stdout:write(rel_path .. "\n")
          end
        end
      elseif verbose then
        -- Dry-run with verbose: show what would be done
        stdout:write(rel_path .. "\n")
      end
    elseif not dry_run then
      -- Create directory
      unix.makedirs(path.join(dest, rel_path))
    end

    ::continue::
  end

  return 0
end

local function cmd_list(opts)
  opts = opts or {}
  local manifest_path = opts.manifest_path or "/zip/MANIFEST.txt"
  local stdout = opts.stdout or io.stdout
  local stderr = opts.stderr or io.stderr
  local verbose = opts.verbose or false
  local null = opts.null or false

  -- Read manifest to get list of files
  local manifest = io.open(manifest_path, "r")
  if not manifest then
    stderr:write("error: failed to read manifest\n")
    return 1
  end

  local files = parse_manifest(manifest:lines())
  manifest:close()

  -- Output each file
  local delimiter = null and string.char(0) or "\n"
  for _, file_info in ipairs(files) do
    local file_path = file_info.path
    local mode = file_info.mode
    local is_dir = is_directory_path(file_path)
    local rel_path = strip_home_prefix(file_path)

    if verbose then
      local mode_str = format_mode(mode, is_dir)
      stdout:write(mode_str .. " " .. rel_path .. delimiter)
    else
      stdout:write(rel_path .. delimiter)
    end
  end

  return 0
end

local function cmd_version(opts)
  opts = opts or {}
  local stdout = opts.stdout or io.stdout
  stdout:write("home built COMMIT_PLACEHOLDER\n")
  return 0
end

local MANAGED_BINARIES = {
  "ast-grep",
  "biome",
  "comrak",
  "delta",
  "duckdb",
  "gh",
  "marksman",
  "nvim",
  "rg",
  "ruff",
  "shfmt",
  "sqruff",
  "stylua",
  "superhtml",
  "tree-sitter",
  "uv",
}

local function find_binary_in_dir(dir, tool_name)
  local patterns = {
    path.join(dir, "bin", tool_name),
    path.join(dir, tool_name),
  }
  for _, p in ipairs(patterns) do
    if unix.stat(p) then
      return p
    end
  end
  return nil
end

local function scan_for_latest_version(tool_name, share_dir)
  share_dir = share_dir or path.join(os.getenv("HOME"), ".local", "share")
  local tool_dir = path.join(share_dir, tool_name)

  if not unix.stat(tool_dir) then
    return nil
  end

  local latest_path = nil
  local latest_version = nil
  local latest_sha = nil

  for name, _ in unix.opendir(tool_dir) do
    if name ~= "." and name ~= ".." then
      local version, sha = name:match("^(.+)%-(%x+)$")
      if version and sha then
        local version_dir = path.join(tool_dir, name)
        local bin_path = find_binary_in_dir(version_dir, tool_name)
        if bin_path and unix.stat(bin_path) then
          if not latest_version or version > latest_version then
            latest_version = version
            latest_sha = sha
            latest_path = bin_path
          end
        end
      end
    end
  end

  if latest_path then
    return {
      version = latest_version,
      sha = latest_sha,
      path = latest_path,
    }
  end
  return nil
end

local function update_symlink(link_path, target_path, opts)
  opts = opts or {}

  if opts.dry_run then
    if opts.verbose then
      opts.stdout:write(string.format("would link %s -> %s\n", link_path, target_path))
    end
    return true
  end

  local st = unix.stat(link_path, unix.AT_SYMLINK_NOFOLLOW)
  if st then
    if unix.S_ISLNK(st:mode()) then
      unix.unlink(link_path)
    else
      return false, "exists and is not a symlink"
    end
  end

  local ok = unix.symlink(target_path, link_path)
  if ok and opts.verbose then
    opts.stdout:write(string.format("%s -> %s\n", link_path, target_path))
  end
  return ok
end

local function cmd_3p(args, opts)
  opts = opts or {}
  local stdout = opts.stdout or io.stdout
  local stderr = opts.stderr or io.stderr
  local verbose = opts.verbose or false
  local dry_run = opts.dry_run or false
  local list_only = args[1] == "list"

  local HOME = opts.home or os.getenv("HOME")
  local bin_dir = path.join(HOME, ".local", "bin")
  local share_dir = opts.share_dir or path.join(HOME, ".local", "share")

  if not dry_run then
    unix.makedirs(bin_dir)
  end

  local results = {}

  for _, tool in ipairs(MANAGED_BINARIES) do
    local info = scan_for_latest_version(tool, share_dir)
    if info then
      table.insert(results, {
        name = tool,
        version = info.version,
        sha = info.sha,
        path = info.path,
      })

      if not list_only then
        local link_path = path.join(bin_dir, tool)
        local ok, err = update_symlink(link_path, info.path, {
          dry_run = dry_run,
          verbose = verbose,
          stdout = stdout,
        })
        if not ok and err then
          stderr:write(string.format("warning: %s: %s\n", tool, err))
        end
      end
    end
  end

  if list_only then
    for _, r in ipairs(results) do
      stdout:write(string.format("%s %s-%s\n", r.name, r.version, r.sha))
    end
  end

  return 0
end

local function cmd_help(opts)
  opts = opts or {}
  local stderr = opts.stderr or io.stderr
  stderr:write("usage: home <command> [options]\n")
  stderr:write("\ncommands:\n")
  stderr:write("  list [options]           - list embedded files (paths only by default)\n")
  stderr:write("  unpack [options] <dest>  - extract dotfiles and binaries to destination\n")
  stderr:write("  3p [subcommand]          - manage third-party binary symlinks\n")
  stderr:write("  version                  - show build version\n")
  stderr:write("\nlist options:\n")
  stderr:write("  --verbose, -v            - show permissions and paths (tar-style)\n")
  stderr:write("  --null, -0               - use null delimiter instead of newline\n")
  stderr:write("\nunpack options:\n")
  stderr:write("  --force, -f              - overwrite existing files\n")
  stderr:write("  --verbose, -v            - show files as they are extracted\n")
  stderr:write("  --dry-run, -n            - show what would be extracted without doing it\n")
  stderr:write("  --only                   - only extract files listed on stdin\n")
  stderr:write("  --null, -0               - read null-delimited paths (with --only)\n")
  stderr:write("\n3p subcommands:\n")
  stderr:write("  3p                       - scan and symlink latest versions\n")
  stderr:write("  3p list                  - list installed tools and versions\n")
  stderr:write("\n3p options:\n")
  stderr:write("  --verbose, -v            - show detailed output\n")
  stderr:write("  --dry-run, -n            - show what would be done\n")
  return 0
end

local function main(args, opts)
  opts = opts or {}
  local parsed = parse_args(args)

  if parsed.cmd == "unpack" then
    -- Merge parsed options with opts
    local unpack_opts = {}
    for k, v in pairs(opts) do
      unpack_opts[k] = v
    end
    unpack_opts.verbose = parsed.verbose
    unpack_opts.dry_run = parsed.dry_run
    unpack_opts.only = parsed.only
    unpack_opts.null = parsed.null

    return cmd_unpack(parsed.dest, parsed.force, unpack_opts)
  elseif parsed.cmd == "list" then
    -- Merge parsed options with opts
    local list_opts = {}
    for k, v in pairs(opts) do
      list_opts[k] = v
    end
    list_opts.verbose = parsed.verbose
    list_opts.null = parsed.null

    return cmd_list(list_opts)
  elseif parsed.cmd == "3p" then
    local threep_opts = {}
    for k, v in pairs(opts) do
      threep_opts[k] = v
    end
    threep_opts.verbose = parsed.verbose
    threep_opts.dry_run = parsed.dry_run

    local subcmd_args = {}
    if parsed.subcmd then
      table.insert(subcmd_args, parsed.subcmd)
    end
    return cmd_3p(subcmd_args, threep_opts)
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
  format_mode = format_mode,
  cmd_unpack = cmd_unpack,
  cmd_list = cmd_list,
  cmd_version = cmd_version,
  cmd_help = cmd_help,
  MANAGED_BINARIES = MANAGED_BINARIES,
  find_binary_in_dir = find_binary_in_dir,
  scan_for_latest_version = scan_for_latest_version,
  update_symlink = update_symlink,
  cmd_3p = cmd_3p,
  main = main,
}

-- Run main if executed directly (not required as module)
if arg and arg[0] and arg[0]:match("/main%.lua$") then
  os.exit(main({ ... }) or 0)
end

return home
