local cosmo = require("cosmo")
local unix = cosmo.unix
local path = cosmo.path

local function read_file(filepath)
  local f, err = io.open(filepath, "rb")
  if not f then
    return nil, "failed to open: " .. (err or "unknown error")
  end
  local data = f:read("*a")
  f:close()
  return data
end

local function load_manifest()
  local ok, manifest = pcall(dofile, "/zip/manifest.lua")
  if ok then
    return manifest
  end
  return nil
end

local function load_platforms()
  local ok, platforms = pcall(dofile, "/zip/platforms.lua")
  if ok then
    return platforms
  end
  return nil
end

local function detect_platform()
  local uname_s = cosmo.uname().sysname or "Linux"
  local uname_m = cosmo.uname().machine or "x86_64"

  if uname_s == "Darwin" then
    return "darwin-arm64"
  elseif uname_m == "aarch64" or uname_m == "arm64" then
    return "linux-arm64"
  else
    return "linux-x86_64"
  end
end

local function interpolate(template, context)
  if type(template) ~= "string" then
    return template
  end
  return template:gsub("%${([%w_]+)}", function(key)
    return tostring(context[key] or "")
  end)
end

local function download_file(url, dest_path, stderr)
  stderr = stderr or io.stderr
  local stdin_r, stdin_w = unix.pipe()
  local stdout_r, stdout_w = unix.pipe()

  local pid = unix.fork()
  if pid == 0 then
    unix.close(stdin_w)
    unix.close(stdout_r)
    unix.dup2(stdin_r, 0)
    unix.dup2(stdout_w, 1)
    unix.dup2(stdout_w, 2)
    unix.close(stdin_r)
    unix.close(stdout_w)
    unix.execve("/usr/bin/curl", {"curl", "-fsSL", "-o", dest_path, url}, unix.environ())
    unix.exit(1)
  end

  unix.close(stdin_r)
  unix.close(stdout_w)
  unix.close(stdin_w)

  while true do
    local chunk = unix.read(stdout_r, 65536)
    if not chunk or chunk == "" then
      break
    end
  end
  unix.close(stdout_r)

  local _, status = unix.wait()
  if status ~= 0 then
    return nil, "curl failed with status " .. tostring(status)
  end
  return true
end

local function verify_sha256(filepath, expected_sha)
  local stdin_r, stdin_w = unix.pipe()
  local stdout_r, stdout_w = unix.pipe()

  local pid = unix.fork()
  if pid == 0 then
    unix.close(stdin_w)
    unix.close(stdout_r)
    unix.dup2(stdin_r, 0)
    unix.dup2(stdout_w, 1)
    unix.close(stdin_r)
    unix.close(stdout_w)
    unix.execve("/usr/bin/shasum", {"shasum", "-a", "256", filepath}, unix.environ())
    unix.exit(1)
  end

  unix.close(stdin_r)
  unix.close(stdout_w)
  unix.close(stdin_w)

  local output = unix.read(stdout_r, 65536) or ""
  unix.close(stdout_r)
  unix.wait()

  local actual = output:match("^(%x+)")
  if not actual or #actual ~= 64 then
    return nil, "failed to compute sha256"
  end
  if actual ~= expected_sha then
    return nil, string.format("sha256 mismatch: expected %s, got %s", expected_sha, actual)
  end
  return true
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

local function parse_args(args)
  local result = {
    cmd = args[1] or "help",
    subcmd = nil,
    force = false,
    verbose = false,
    dry_run = false,
    only = false,
    null = false,
    with_binaries = false,
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
    elseif args[i] == "--with-binaries" then
      result.with_binaries = true
    elseif result.cmd == "3p" and not result.subcmd and not args[i]:match("^%-") then
      result.subcmd = args[i]
    elseif not result.dest then
      result.dest = args[i]
    end
    i = i + 1
  end

  return result
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

local function extract_platform_asset(asset_path, dest, force, filter, opts)
  opts = opts or {}
  local stderr = opts.stderr or io.stderr
  local stdout = opts.stdout or io.stdout
  local verbose = opts.verbose or false
  local dry_run = opts.dry_run or false

  local stdin_r, stdin_w = unix.pipe()
  local stdout_r, stdout_w = unix.pipe()

  local cmd_args = {"unpack"}
  if force then
    table.insert(cmd_args, "--force")
  end
  if verbose then
    table.insert(cmd_args, "--verbose")
  end
  if dry_run then
    table.insert(cmd_args, "--dry-run")
  end
  if filter then
    table.insert(cmd_args, "--only")
  end
  table.insert(cmd_args, dest)
  table.insert(cmd_args, 1, asset_path)

  local pid = unix.fork()
  if pid == 0 then
    unix.close(stdin_w)
    unix.close(stdout_r)
    unix.dup2(stdin_r, 0)
    unix.dup2(stdout_w, 1)
    unix.dup2(stdout_w, 2)
    unix.close(stdin_r)
    unix.close(stdout_w)
    unix.execve(asset_path, cmd_args, unix.environ())
    unix.exit(1)
  end

  unix.close(stdin_r)
  unix.close(stdout_w)

  if filter then
    local filter_lines = {}
    for p in pairs(filter) do
      table.insert(filter_lines, p)
    end
    unix.write(stdin_w, table.concat(filter_lines, "\n"))
  end
  unix.close(stdin_w)

  while true do
    local chunk = unix.read(stdout_r, 65536)
    if not chunk or chunk == "" then
      break
    end
    stdout:write(chunk)
  end
  unix.close(stdout_r)

  local _, status = unix.wait()
  return status == 0
end

local function cmd_unpack(dest, force, opts)
  opts = opts or {}
  local stderr = opts.stderr or io.stderr
  local stdout = opts.stdout or io.stdout
  local verbose = opts.verbose or false
  local dry_run = opts.dry_run or false
  local only = opts.only or false
  local with_binaries = opts.with_binaries or false

  if not dest then
    stderr:write("error: destination path required\n")
    stderr:write("usage: home unpack [--force] [--with-binaries] <destination>\n")
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

  local manifest = load_manifest()
  if not manifest or not manifest.files then
    stderr:write("error: failed to load manifest\n")
    return 1
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

  if with_binaries then
    local platforms = load_platforms()
    if not platforms then
      stderr:write("error: no platform metadata available\n")
      return 1
    end

    local current = detect_platform()
    local plat_info = platforms.platforms and platforms.platforms[current]
    if not plat_info then
      stderr:write("error: no binaries available for " .. current .. "\n")
      return 1
    end

    local url = interpolate(platforms.base_url, {tag = platforms.tag}) .. "/" .. plat_info.asset
    local tmp_path = path.join(dest, ".home-binaries-download")

    if not dry_run then
      if verbose then
        stdout:write("downloading " .. plat_info.asset .. "...\n")
      end

      local ok, err = download_file(url, tmp_path, stderr)
      if not ok then
        stderr:write("error: download failed: " .. (err or "unknown") .. "\n")
        return 1
      end

      ok, err = verify_sha256(tmp_path, plat_info.sha256)
      if not ok then
        stderr:write("error: " .. (err or "checksum failed") .. "\n")
        unix.unlink(tmp_path)
        return 1
      end

      unix.chmod(tmp_path, 493)

      local bin_filter = nil
      if filter and plat_info.manifest and plat_info.manifest.files then
        bin_filter = {}
        for p in pairs(plat_info.manifest.files) do
          if filter[p] then
            bin_filter[p] = true
          end
        end
      end

      ok = extract_platform_asset(tmp_path, dest, force, bin_filter, {
        stderr = stderr,
        stdout = stdout,
        verbose = verbose,
        dry_run = dry_run,
      })

      unix.unlink(tmp_path)

      if not ok then
        stderr:write("error: failed to extract platform binaries\n")
        return 1
      end
    elseif verbose then
      stdout:write("would download " .. plat_info.asset .. "\n")
      if plat_info.manifest and plat_info.manifest.files then
        local bin_paths = {}
        for p in pairs(plat_info.manifest.files) do
          if not filter or filter[p] then
            table.insert(bin_paths, p)
          end
        end
        table.sort(bin_paths)
        for _, p in ipairs(bin_paths) do
          stdout:write(p .. "\n")
        end
      end
    end
  end

  return 0
end

local function cmd_list(opts)
  opts = opts or {}
  local stdout = opts.stdout or io.stdout
  local stderr = opts.stderr or io.stderr
  local verbose = opts.verbose or false
  local null = opts.null or false

  local manifest = load_manifest()
  if not manifest or not manifest.files then
    stderr:write("error: failed to load manifest\n")
    return 1
  end

  local delimiter = null and string.char(0) or "\n"
  local all_paths = {}

  for p, info in pairs(manifest.files) do
    table.insert(all_paths, {path = p, mode = info.mode, source = "dotfiles"})
  end

  local platforms = load_platforms()
  if platforms and platforms.platforms then
    local current = detect_platform()
    local plat_info = platforms.platforms[current]
    if plat_info and plat_info.manifest and plat_info.manifest.files then
      for p, info in pairs(plat_info.manifest.files) do
        table.insert(all_paths, {path = p, mode = info.mode, source = "binaries"})
      end
    end
  end

  table.sort(all_paths, function(a, b) return a.path < b.path end)

  for _, entry in ipairs(all_paths) do
    if verbose then
      local mode_str = format_mode(entry.mode, false)
      stdout:write(mode_str .. " " .. entry.path .. delimiter)
    else
      stdout:write(entry.path .. delimiter)
    end
  end

  return 0
end

local function cmd_version(opts)
  opts = opts or {}
  local stdout = opts.stdout or io.stdout
  local manifest = load_manifest()
  if manifest and manifest.version then
    stdout:write("home built " .. manifest.version .. "\n")
  else
    stdout:write("home built COMMIT_PLACEHOLDER\n")
  end
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
  stderr:write("  list [options]           list embedded files\n")
  stderr:write("  unpack [options] <dest>  extract dotfiles to destination\n")
  stderr:write("  3p [subcommand]          manage third-party binary symlinks\n")
  stderr:write("  version                  show build version\n")
  stderr:write("\nlist options:\n")
  stderr:write("  --verbose, -v            show permissions\n")
  stderr:write("  --null, -0               use null delimiter\n")
  stderr:write("\nunpack options:\n")
  stderr:write("  --force, -f              overwrite existing files\n")
  stderr:write("  --verbose, -v            show files as extracted\n")
  stderr:write("  --dry-run, -n            show what would be extracted\n")
  stderr:write("  --with-binaries          download and extract platform binaries\n")
  stderr:write("  --only                   only extract files listed on stdin\n")
  stderr:write("  --null, -0               read null-delimited paths (with --only)\n")
  stderr:write("\n3p subcommands:\n")
  stderr:write("  3p                       scan and symlink latest versions\n")
  stderr:write("  3p list                  list installed tools and versions\n")
  stderr:write("\n3p options:\n")
  stderr:write("  --verbose, -v            show detailed output\n")
  stderr:write("  --dry-run, -n            show what would be done\n")
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
    unpack_opts.with_binaries = parsed.with_binaries

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
  load_manifest = load_manifest,
  load_platforms = load_platforms,
  detect_platform = detect_platform,
  download_file = download_file,
  verify_sha256 = verify_sha256,
  copy_file = copy_file,
  parse_args = parse_args,
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
