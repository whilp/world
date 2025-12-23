local cosmo = require("cosmo")
local unix = cosmo.unix
local path = cosmo.path

local function copy_file(src, dst, mode, overwrite)
  local src_f, err = io.open(src, "rb")
  if not src_f then
    return false, "failed to open source: " .. (err or "unknown error")
  end
  local data = src_f:read("*a")
  src_f:close()

  if overwrite then
    local st = unix.stat(dst, unix.AT_SYMLINK_NOFOLLOW)
    if st and unix.S_ISLNK(st:mode()) then
      local unlink_ok = unix.unlink(dst)
      if not unlink_ok then
        return false, "failed to remove existing symlink"
      end
    end
  end

  local flags = unix.O_WRONLY | unix.O_CREAT
  if overwrite then
    flags = flags | unix.O_TRUNC
  else
    flags = flags | unix.O_EXCL
  end

  local fd = unix.open(dst, flags, 384)
  if not fd or fd < 0 then
    if overwrite then
      return false, "failed to open destination for writing"
    else
      return false, "destination already exists (use --force to overwrite)"
    end
  end

  local bytes_written = unix.write(fd, data)
  local success = bytes_written == #data

  if not success then
    unix.close(fd)
    return false, "failed to write data (wrote " .. bytes_written .. " of " .. #data .. " bytes)"
  end

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

local function load_manifest()
  local ok, manifest = pcall(dofile, "/zip/manifest.lua")
  if ok then
    return manifest
  end
  return nil
end

local function format_mode(mode, is_dir)
  if not mode then
    return "----------"
  end

  local result = {}
  table.insert(result, is_dir and "d" or "-")

  for i = 2, 0, -1 do
    local shift = i * 3
    local perms = (mode >> shift) & 7
    table.insert(result, (perms & 4) ~= 0 and "r" or "-")
    table.insert(result, (perms & 2) ~= 0 and "w" or "-")
    table.insert(result, (perms & 1) ~= 0 and "x" or "-")
  end

  return table.concat(result)
end

local function parse_args(args)
  local result = {
    cmd = args[1] or "help",
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
    elseif not result.dest then
      result.dest = args[i]
    end
    i = i + 1
  end

  return result
end

local function cmd_list(opts)
  opts = opts or {}
  local stdout = opts.stdout or io.stdout
  local stderr = opts.stderr or io.stderr
  local verbose = opts.verbose or false
  local null = opts.null or false

  local manifest = load_manifest()
  if not manifest then
    stderr:write("error: failed to load manifest\n")
    return 1
  end

  local delimiter = null and string.char(0) or "\n"

  local paths = {}
  if manifest.files then
    for p in pairs(manifest.files) do
      table.insert(paths, p)
    end
  end
  table.sort(paths)

  for _, p in ipairs(paths) do
    local info = manifest.files[p]
    if verbose then
      local mode_str = format_mode(info.mode, false)
      stdout:write(mode_str .. " " .. p .. delimiter)
    else
      stdout:write(p .. delimiter)
    end
  end

  return 0
end

local function cmd_unpack(dest, force, opts)
  opts = opts or {}
  local stderr = opts.stderr or io.stderr
  local stdout = opts.stdout or io.stdout
  local verbose = opts.verbose or false
  local dry_run = opts.dry_run or false
  local only = opts.only or false

  if not dest then
    stderr:write("error: destination path required\n")
    stderr:write("usage: home-<platform> unpack [--force] <destination>\n")
    return 1
  end

  local manifest = load_manifest()
  if not manifest or not manifest.files then
    stderr:write("error: failed to load manifest\n")
    return 1
  end

  local filter = nil
  if only then
    filter = {}
    local input = opts.filter_input or io.stdin:read("*a")
    local pattern = opts.null and "[^\0]+" or "[^\n]+"
    for line in input:gmatch(pattern) do
      local p = line:match("^%s*(.-)%s*$")
      if p and p ~= "" then
        filter[p] = true
      end
    end
  end

  if not dry_run then
    if not unix.makedirs(dest) then
      stderr:write("error: failed to create destination directory\n")
      return 1
    end
  end

  local paths = {}
  for p in pairs(manifest.files) do
    table.insert(paths, p)
  end
  table.sort(paths)

  for _, rel_path in ipairs(paths) do
    local info = manifest.files[rel_path]
    local zip_path = "/zip/home/" .. rel_path
    local dest_path = path.join(dest, rel_path)

    if filter and not filter[rel_path] then
      goto continue
    end

    local file_exists = not dry_run and unix.stat(dest_path) ~= nil

    if not dry_run then
      local parent = path.dirname(dest_path)
      unix.makedirs(parent)

      local ok, err = copy_file(zip_path, dest_path, info.mode, force)
      if not ok then
        stderr:write("warning: failed to copy " .. rel_path .. ": " .. (err or "unknown error") .. "\n")
      elseif verbose then
        if force and file_exists then
          stdout:write(rel_path .. " (overwritten)\n")
        else
          stdout:write(rel_path .. "\n")
        end
      end
    elseif verbose then
      stdout:write(rel_path .. "\n")
    end

    ::continue::
  end

  return 0
end

local function cmd_version(opts)
  opts = opts or {}
  local stdout = opts.stdout or io.stdout
  local manifest = load_manifest()
  if manifest and manifest.version then
    stdout:write("home-platform built " .. manifest.version .. "\n")
  else
    stdout:write("home-platform built COMMIT_PLACEHOLDER\n")
  end
  return 0
end

local function cmd_help(opts)
  opts = opts or {}
  local stderr = opts.stderr or io.stderr
  stderr:write("usage: home-<platform> <command> [options]\n")
  stderr:write("\n")
  stderr:write("commands:\n")
  stderr:write("  list [options]           list embedded files\n")
  stderr:write("  unpack [options] <dest>  extract binaries to destination\n")
  stderr:write("  version                  show build version\n")
  stderr:write("\n")
  stderr:write("list options:\n")
  stderr:write("  --verbose, -v            show permissions\n")
  stderr:write("  --null, -0               use null delimiter\n")
  stderr:write("\n")
  stderr:write("unpack options:\n")
  stderr:write("  --force, -f              overwrite existing files\n")
  stderr:write("  --verbose, -v            show files as extracted\n")
  stderr:write("  --dry-run, -n            show what would be extracted\n")
  stderr:write("  --only                   only extract files listed on stdin\n")
  stderr:write("  --null, -0               read null-delimited paths (with --only)\n")
  return 0
end

local function main(args, opts)
  opts = opts or {}
  local parsed = parse_args(args)

  if parsed.cmd == "unpack" then
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
    local list_opts = {}
    for k, v in pairs(opts) do
      list_opts[k] = v
    end
    list_opts.verbose = parsed.verbose
    list_opts.null = parsed.null
    return cmd_list(list_opts)
  elseif parsed.cmd == "version" then
    return cmd_version(opts)
  elseif parsed.cmd == "help" then
    return cmd_help(opts)
  else
    cmd_help(opts)
    return 1
  end
end

local M = {
  copy_file = copy_file,
  load_manifest = load_manifest,
  format_mode = format_mode,
  parse_args = parse_args,
  cmd_list = cmd_list,
  cmd_unpack = cmd_unpack,
  cmd_version = cmd_version,
  cmd_help = cmd_help,
  main = main,
}

if arg and arg[0] and arg[0]:match("/platform%-main%.lua$") then
  os.exit(main({ ... }) or 0)
end

return M
