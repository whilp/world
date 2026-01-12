-- home binary: manages dotfiles extraction and platform assets
local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

-- Platform architecture mapping
local type ArchMap = {string:string}

-- System to platform mapping
local type PlatformMap = {string:ArchMap}

-- Platform normalization table
local PLATFORMS: PlatformMap = {
  ["Darwin"] = {
    ["arm64"] = "darwin-arm64",
    ["aarch64"] = "darwin-arm64",
  },
  -- cosmo.GetHostOs() returns "XNU" on macOS (the kernel name)
  ["Xnu"] = {
    ["arm64"] = "darwin-arm64",
    ["aarch64"] = "darwin-arm64",
  },
  ["Linux"] = {
    ["x86_64"] = "linux-x86_64",
    ["aarch64"] = "linux-arm64",
    ["arm64"] = "linux-arm64",
  },
}

local function read_file(filepath: string): string, string
  local f, err = io.open(filepath, "rb")
  if not f then
    return nil, "failed to open: " .. (err or "unknown error")
  end
  local data = f:read("*a")
  f:close()
  return data
end

local function serialize_value(val: any, indent: string): string
  indent = indent or ""
  local t = type(val)

  if t == "string" then
    return string.format("%q", val)
  elseif t == "number" then
    return tostring(val)
  elseif t == "boolean" then
    return val and "true" or "false"
  elseif t == "nil" then
    return "nil"
  elseif t == "table" then
    local tbl = val as {any:any}
    local lines: {string} = {}
    local next_indent = indent .. "  "

    local is_array = true
    local max_index = 0
    for k, _ in pairs(tbl) do
      if type(k) ~= "number" or (k as number) < 1 or k ~= math.floor(k as number) then
        is_array = false
        break
      end
      if (k as number) > max_index then
        max_index = k as number
      end
    end
    local arr = tbl as {any}
    if is_array and max_index ~= #arr then
      is_array = false
    end

    table.insert(lines, "{")

    if is_array then
      for i, v in ipairs(arr) do
        local comma = i < #arr and "," or ""
        table.insert(lines, next_indent .. serialize_value(v, next_indent) .. comma)
      end
    else
      local keys: {any} = {}
      for k in pairs(tbl) do
        table.insert(keys, k)
      end
      table.sort(keys, function(a: any, b: any): boolean
        if type(a) == type(b) then
          return (a as string) < (b as string)
        end
        return type(a) < type(b)
      end)

      for i, k in ipairs(keys) do
        local v = tbl[k]
        local key_str: string
        if type(k) == "string" and (k as string):match("^[a-zA-Z_][a-zA-Z0-9_]*$") then
          key_str = k as string
        else
          key_str = "[" .. serialize_value(k, next_indent) .. "]"
        end
        local comma = i < #keys and "," or ""
        table.insert(lines, next_indent .. key_str .. " = " .. serialize_value(v, next_indent) .. comma)
      end
    end

    table.insert(lines, indent .. "}")
    return table.concat(lines, "\n")
  else
    return "nil"
  end
end

local function serialize_table(tbl: {any:any}): string
  return "return " .. serialize_value(tbl) .. "\n"
end

local function detect_platform(): string, string
  -- cosmo returns uppercase (LINUX, DARWIN, X86_64, AARCH64)
  -- normalize to match PLATFORMS table keys
  local host_os = cosmo.GetHostOs()
  local sysname = host_os:sub(1, 1) .. host_os:sub(2):lower()
  local machine = cosmo.GetHostIsa():lower()

  local sys_platforms = PLATFORMS[sysname]
  if not sys_platforms then
    return nil, "unsupported system: " .. sysname
  end

  local platform = sys_platforms[machine]
  if not platform then
    return nil, "unsupported machine: " .. machine
  end

  return platform
end

-- Manifest file info
local record FileInfo
  mode: number
end

-- Main manifest structure
local record Manifest
  version: string
  files: {string:FileInfo}
end

-- Platform asset info
local record PlatformAsset
  asset: string
  sha256: string
end

-- Platforms metadata
local record Platforms
  base_url: string
  tag: string
  platforms: {string:PlatformAsset}
end

local function load_manifest(): Manifest
  local ok, manifest = pcall(dofile, "/zip/manifest.lua")
  if ok then
    return manifest as Manifest
  end
  return nil
end

local function load_platforms(): Platforms
  local ok, platforms = pcall(dofile, "/zip/platforms.lua")
  if ok then
    return platforms as Platforms
  end
  return nil
end

local function load_platform_manifest(platform: string): Manifest
  local ok, manifest = pcall(dofile, "/zip/manifests/" .. platform .. ".lua")
  if ok then
    return manifest as Manifest
  end
  return nil
end

local function is_platform_mode(): boolean
  local ok, _ = pcall(dofile, "/zip/platforms.lua")
  return not ok
end

local function interpolate(template: string, context: {string:string}): string
  if type(template) ~= "string" then
    return template
  end
  return template:gsub("{([%w_]+)}", function(key: string): string
    return tostring(context[key] or "")
  end)
end

local function sha256_file(filepath: string): string, string
  local shasum = unix.commandv("shasum")
  local cmd_args: {string}
  if shasum then
    cmd_args = {"shasum", "-a", "256", filepath}
  elseif unix.commandv("sha256sum") then
    cmd_args = {"sha256sum", filepath}
  else
    return nil, "neither shasum nor sha256sum found"
  end

  local ok, output = spawn(cmd_args):read()
  if not ok then
    return nil, "failed to compute sha256"
  end

  local sha = (output as string):match("^(%x+)")
  if not sha or #sha ~= 64 then
    return nil, "invalid sha256 output"
  end
  return sha
end

local function download_file(url: string, dest_path: string): boolean, string
  local status, _, body = cosmo.Fetch(url, {maxresponse = 300 * 1024 * 1024})
  if not status then
    return nil, "fetch failed: " .. tostring(body or "unknown error")
  end
  if status ~= 200 then
    return nil, "fetch failed with status " .. tostring(status)
  end

  local fd = unix.open(dest_path, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("0644", 8))
  if not fd or fd < 0 then
    return nil, "failed to open destination file"
  end
  local bytes_written = unix.write(fd, body)
  unix.close(fd)
  if bytes_written ~= #body then
    return nil, "failed to write data"
  end
  return true
end

local function verify_sha256(filepath: string, expected_sha: string): boolean, string
  local actual, err = sha256_file(filepath)
  if not actual then
    return nil, err
  end
  if actual ~= expected_sha then
    return nil, string.format("sha256 mismatch: expected %s, got %s", expected_sha, actual)
  end
  return true
end

local function copy_file(src: string, dst: string, mode: number, overwrite: boolean): boolean, string
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

  local fd = unix.open(dst, flags, tonumber("0600", 8))
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

local function format_mode(mode: number, is_dir: boolean): string
  if not mode then
    return "----------"
  end

  local result: {string} = {}
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

-- Parsed command line arguments
local record ParsedArgs
  cmd: string
  subcmd: string
  force: boolean
  verbose: boolean
  dry_run: boolean
  only: boolean
  null: boolean
  with_platform: boolean
  platform_url: string
  platform: string
  dest: string
end

local function parse_args(args: {string}): ParsedArgs
  local result: ParsedArgs = {
    cmd = args[1] or "help",
    subcmd = nil,
    force = false,
    verbose = false,
    dry_run = false,
    only = false,
    null = false,
    with_platform = false,
    platform_url = nil,
    platform = nil,
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
    elseif args[i] == "--with-platform" then
      result.with_platform = true
    elseif args[i] == "--platform-url" then
      i = i + 1
      result.platform_url = args[i]
    elseif args[i] == "--platform" then
      i = i + 1
      result.platform = args[i]
    elseif result.cmd == "mac" and not result.subcmd and not args[i]:match("^%-") then
      result.subcmd = args[i]
    elseif not result.dest then
      result.dest = args[i]
    end
    i = i + 1
  end

  return result
end

-- Options for extract_platform_asset
local record ExtractOpts
  stdout: FILE
  stderr: FILE
  verbose: boolean
  dry_run: boolean
end

local function create_versioned_symlinks(base_dir: string, verbose: boolean, stderr: FILE)
  local version_mod = require("version")
  local base_stat = unix.stat(base_dir)
  if not base_stat or not unix.S_ISDIR(base_stat:mode()) then
    return
  end

  local entries = unix.opendir(base_dir)
  if not entries then
    return
  end

  for entry_name in entries do
    if entry_name ~= "." and entry_name ~= ".." then
      local entry_path = path.join(base_dir, entry_name)
      local entry_stat = unix.stat(entry_path)
      if entry_stat and unix.S_ISDIR(entry_stat:mode()) then
        -- Find versioned directory (contains version-sha pattern)
        local versioned_dir: string = nil
        local sub_entries = unix.opendir(entry_path)
        if sub_entries then
          for sub_entry in sub_entries do
            if sub_entry ~= "." and sub_entry ~= ".." and version_mod.is_version_dir(sub_entry) then
              versioned_dir = sub_entry
              break
            end
          end
        end

        -- Create symlinks from entry root to versioned subdirectory contents
        if versioned_dir then
          local versioned_path = path.join(entry_path, versioned_dir)
          local versioned_entries = unix.opendir(versioned_path)
          if versioned_entries then
            for item in versioned_entries do
              if item ~= "." and item ~= ".." then
                local link_target = path.join(versioned_dir, item)
                local link_path = path.join(entry_path, item)
                local link_stat = unix.stat(link_path, unix.AT_SYMLINK_NOFOLLOW)
                if not link_stat then
                  local ok = unix.symlink(link_target, link_path)
                  if not ok and verbose then
                    stderr:write("warning: failed to create symlink " .. link_path .. "\n")
                  end
                end
              end
            end
          end
        end
      end
    end
  end
end

local function extract_platform_asset(asset_path: string, dest: string, force: boolean, filter: {string:boolean}, opts: ExtractOpts): boolean
  opts = opts or {} as ExtractOpts
  local stdout = opts.stdout or io.stdout
  local verbose = opts.verbose or false
  local dry_run = opts.dry_run or false

  local cmd_args: {string} = { asset_path, "unpack" }
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

  local stdin_data: string = nil
  if filter then
    local filter_lines: {string} = {}
    for p in pairs(filter) do
      table.insert(filter_lines, p)
    end
    stdin_data = table.concat(filter_lines, "\n")
  end

  local ok, output = spawn(cmd_args, {stdin = stdin_data}):read()
  if output and output ~= "" then
    stdout:write(output as string)
  end
  return ok as boolean
end

-- Options for cmd_unpack
local record UnpackOpts
  stderr: FILE
  stdout: FILE
  verbose: boolean
  dry_run: boolean
  only: boolean
  null: boolean
  with_platform: boolean
  platform_url: string
  platform: string
  zip_root: string
  filter_input: string
  manifest: Manifest
end

local function cmd_unpack(dest: string, force: boolean, opts: UnpackOpts): integer
  opts = opts or {} as UnpackOpts
  local stderr = opts.stderr or io.stderr
  local stdout = opts.stdout or io.stdout
  local verbose = opts.verbose or false
  local dry_run = opts.dry_run or false
  local only = opts.only or false
  local with_platform = opts.with_platform or false
  local platform_url = opts.platform_url
  local platform_override = opts.platform
  local zip_root = opts.zip_root or "/zip/home/"

  if not dest then
    stderr:write("error: destination path required\n")
    stderr:write("usage: home unpack [--force] [--with-platform] <destination>\n")
    return 1
  end

  local filter: {string:boolean} = nil
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

  local manifest = opts.manifest or load_manifest()
  if not manifest or not manifest.files then
    stderr:write("error: failed to load manifest\n")
    return 1
  end

  local paths: {string} = {}
  for p in pairs(manifest.files) do
    table.insert(paths, p)
  end
  table.sort(paths)

  for _, rel_path in ipairs(paths) do
    local info = manifest.files[rel_path]
    local zip_path = zip_root .. rel_path
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

  -- Create symlinks from versioned subdirectories in .local/share and .local/lib
  if not dry_run then
    create_versioned_symlinks(path.join(dest, ".local", "share"), verbose, stderr)
    create_versioned_symlinks(path.join(dest, ".local", "lib"), verbose, stderr)
  end

  if with_platform then
    local platforms = load_platforms()
    if not platforms then
      stderr:write("error: no platform metadata available\n")
      return 1
    end

    local current: string
    local err: string
    if platform_override then
      current = platform_override
    else
      current, err = detect_platform()
      if not current then
        stderr:write("error: " .. (err or "failed to detect platform") .. "\n")
        return 1
      end
    end

    local plat_info = platforms.platforms and platforms.platforms[current]
    if not plat_info then
      stderr:write("error: no platform asset available for " .. current .. "\n")
      return 1
    end

    local plat_manifest = load_platform_manifest(current)
    local base_url = interpolate(platforms.base_url, { tag = platforms.tag })
    local url = platform_url or string.format("%s/%s", base_url, plat_info.asset)
    local tmp_path = path.join(dest, ".home-platform-download")

    if not dry_run then
      if verbose then
        stdout:write("downloading " .. plat_info.asset .. "...\n")
      end

      local ok: boolean
      ok, err = download_file(url, tmp_path)
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

      unix.chmod(tmp_path, tonumber("0755", 8))

      local bin_filter: {string:boolean} = nil
      if filter and plat_manifest and plat_manifest.files then
        bin_filter = {}
        for p in pairs(plat_manifest.files) do
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
      } as ExtractOpts)

      unix.unlink(tmp_path)

      if not ok then
        stderr:write("error: failed to extract platform asset\n")
        return 1
      end
    elseif verbose then
      stdout:write("would download " .. plat_info.asset .. "\n")
      if plat_manifest and plat_manifest.files then
        local bin_paths: {string} = {}
        for p in pairs(plat_manifest.files) do
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

-- Options for cmd_list
local record ListOpts
  stdout: FILE
  stderr: FILE
  verbose: boolean
  null: boolean
  manifest: Manifest
end

-- Path entry for list output
local record PathEntry
  path: string
  mode: number
  source: string
end

local function cmd_list(opts: ListOpts): integer
  opts = opts or {} as ListOpts
  local stdout = opts.stdout or io.stdout
  local stderr = opts.stderr or io.stderr
  local verbose = opts.verbose or false
  local null = opts.null or false

  local manifest = opts.manifest or load_manifest()
  if not manifest or not manifest.files then
    stderr:write("error: failed to load manifest\n")
    return 1
  end

  local delimiter = null and string.char(0) or "\n"
  local all_paths: {PathEntry} = {}

  for p, info in pairs(manifest.files) do
    table.insert(all_paths, { path = p, mode = info.mode, source = "dotfiles" })
  end

  if not is_platform_mode() then
    local current = detect_platform()
    if current then
      local plat_manifest = load_platform_manifest(current)
      if plat_manifest and plat_manifest.files then
        for p, info in pairs(plat_manifest.files) do
          table.insert(all_paths, { path = p, mode = info.mode, source = "platform" })
        end
      end
    end
  end

  table.sort(all_paths, function(a: PathEntry, b: PathEntry): boolean
    return a.path < b.path
  end)

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

-- Options for cmd_version
local record VersionOpts
  stdout: FILE
end

local function cmd_version(opts: VersionOpts): integer
  opts = opts or {} as VersionOpts
  local stdout = opts.stdout or io.stdout
  local manifest = load_manifest()
  if manifest and manifest.version then
    stdout:write("home " .. manifest.version .. "\n")
  else
    stdout:write("home (unknown version)\n")
  end
  return 0
end

-- Options for cmd_setup
local record SetupOpts
  stderr: FILE
end

-- Setup module interface
local record SetupModule
  main: function(): any
end

local function cmd_setup(dest: string, opts: SetupOpts): integer
  opts = opts or {} as SetupOpts
  local stderr = opts.stderr or io.stderr

  if not dest then
    stderr:write("error: destination path required\n")
    stderr:write("usage: home setup <destination>\n")
    return 1
  end

  local ok, setup_module = pcall(require, "setup.setup")
  if not ok then
    stderr:write("error: failed to load setup module: " .. tostring(setup_module) .. "\n")
    return 1
  end

  local success, err = pcall((setup_module as SetupModule).main)
  if not success then
    stderr:write("error: setup failed: " .. tostring(err) .. "\n")
    return 1
  end

  return 0
end

-- Options for cmd_mac
local record MacOpts
  stdout: FILE
  stderr: FILE
end

-- Mac module interface
local record MacModule
  list: function(): {string}
  run_script: function(name: string): integer
  run_all: function(): integer
end

local function cmd_mac(args: {string}, opts: MacOpts): integer
  opts = opts or {} as MacOpts
  local stdout = opts.stdout or io.stdout
  local stderr = opts.stderr or io.stderr

  local ok, mac_module = pcall(require, "mac")
  if not ok then
    stderr:write("error: failed to load mac module: " .. tostring(mac_module) .. "\n")
    return 1
  end

  local mac = mac_module as MacModule
  local subcmd = args[1]

  if subcmd == "list" then
    for _, name in ipairs(mac.list()) do
      stdout:write(name .. "\n")
    end
    return 0
  elseif subcmd then
    return mac.run_script(subcmd)
  else
    return mac.run_all()
  end
end

-- Options for cmd_help
local record HelpOpts
  stderr: FILE
end

local function cmd_help(opts: HelpOpts): integer
  opts = opts or {} as HelpOpts
  local stderr = opts.stderr or io.stderr
  local platform_mode = is_platform_mode()

  stderr:write("usage: home <command> [options]\n")
  stderr:write("\ncommands:\n")
  stderr:write("  list [options]           list embedded files\n")
  stderr:write("  unpack [options] <dest>  extract files to destination\n")
  if not platform_mode then
    stderr:write("  setup <dest>             run setup scripts\n")
    stderr:write("  mac [script]             run macOS defaults scripts\n")
  end
  stderr:write("  version                  show build version\n")
  stderr:write("\nlist options:\n")
  stderr:write("  --verbose, -v            show permissions\n")
  stderr:write("  --null, -0               use null delimiter\n")
  stderr:write("\nunpack options:\n")
  stderr:write("  --force, -f              overwrite existing files\n")
  stderr:write("  --verbose, -v            show files as extracted\n")
  stderr:write("  --dry-run, -n            show what would be extracted\n")
  if not platform_mode then
    stderr:write("  --with-platform          download and extract platform binaries\n")
    stderr:write("  --platform <name>        override platform detection\n")
    stderr:write("  --platform-url <url>     override platform asset download url\n")
  end
  stderr:write("  --only                   only extract files listed on stdin\n")
  stderr:write("  --null, -0               read null-delimited paths (with --only)\n")
  if not platform_mode then
    stderr:write("\nmac subcommands:\n")
    stderr:write("  mac                      run all macOS defaults scripts\n")
    stderr:write("  mac list                 list available scripts\n")
    stderr:write("  mac <script>             run a specific script\n")
  end
  return 0
end

-- Main options
local record MainOpts
  stderr: FILE
  stdout: FILE
  manifest: Manifest
  zip_root: string
  filter_input: string
end

local function main(args: {string}, opts: MainOpts): integer
  opts = opts or {} as MainOpts
  local parsed = parse_args(args)

  if parsed.cmd == "unpack" then
    local unpack_opts: UnpackOpts = {} as UnpackOpts
    for k, v in pairs(opts as {string:any}) do
      (unpack_opts as {string:any})[k] = v
    end
    unpack_opts.verbose = parsed.verbose
    unpack_opts.dry_run = parsed.dry_run
    unpack_opts.only = parsed.only
    unpack_opts.null = parsed.null
    unpack_opts.with_platform = parsed.with_platform
    unpack_opts.platform_url = parsed.platform_url
    unpack_opts.platform = parsed.platform

    return cmd_unpack(parsed.dest, parsed.force, unpack_opts)
  elseif parsed.cmd == "list" then
    local list_opts: ListOpts = {} as ListOpts
    for k, v in pairs(opts as {string:any}) do
      (list_opts as {string:any})[k] = v
    end
    list_opts.verbose = parsed.verbose
    list_opts.null = parsed.null

    return cmd_list(list_opts)
  elseif parsed.cmd == "setup" then
    return cmd_setup(parsed.dest, opts as SetupOpts)
  elseif parsed.cmd == "mac" then
    local mac_args: {string} = {}
    if parsed.subcmd then
      table.insert(mac_args, parsed.subcmd)
    elseif parsed.dest then
      table.insert(mac_args, parsed.dest)
    end
    return cmd_mac(mac_args, opts as MacOpts)
  elseif parsed.cmd == "version" then
    return cmd_version(opts as VersionOpts)
  elseif parsed.cmd == "help" then
    return cmd_help(opts as HelpOpts)
  else
    cmd_help(opts as HelpOpts)
    return 1
  end
end

-- Export type for spawn module
local type SpawnModule = function(argv: {string}, opts?: {string:any}): any

-- Module record
local record Home
  PLATFORMS: PlatformMap
  read_file: function(filepath: string): string, string
  spawn: SpawnModule
  serialize_value: function(val: any, indent: string): string
  serialize_table: function(tbl: {any:any}): string
  detect_platform: function(): string, string
  load_manifest: function(): Manifest
  load_platforms: function(): Platforms
  load_platform_manifest: function(platform: string): Manifest
  is_platform_mode: function(): boolean
  sha256_file: function(filepath: string): string, string
  download_file: function(url: string, dest_path: string): boolean, string
  verify_sha256: function(filepath: string, expected_sha: string): boolean, string
  copy_file: function(src: string, dst: string, mode: number, overwrite: boolean): boolean, string
  format_mode: function(mode: number, is_dir: boolean): string
  parse_args: function(args: {string}): ParsedArgs
  cmd_unpack: function(dest: string, force: boolean, opts: UnpackOpts): integer
  cmd_list: function(opts: ListOpts): integer
  cmd_setup: function(dest: string, opts: SetupOpts): integer
  cmd_mac: function(args: {string}, opts: MacOpts): integer
  cmd_version: function(opts: VersionOpts): integer
  cmd_help: function(opts: HelpOpts): integer
  main: function(args: {string}, opts: MainOpts): integer
end

local home: Home = {
  PLATFORMS = PLATFORMS,
  read_file = read_file,
  spawn = spawn as SpawnModule,
  serialize_value = serialize_value,
  serialize_table = serialize_table,
  detect_platform = detect_platform,
  load_manifest = load_manifest,
  load_platforms = load_platforms,
  load_platform_manifest = load_platform_manifest,
  is_platform_mode = is_platform_mode,
  sha256_file = sha256_file,
  download_file = download_file,
  verify_sha256 = verify_sha256,
  copy_file = copy_file,
  format_mode = format_mode,
  parse_args = parse_args,
  cmd_unpack = cmd_unpack,
  cmd_list = cmd_list,
  cmd_setup = cmd_setup,
  cmd_mac = cmd_mac,
  cmd_version = cmd_version,
  cmd_help = cmd_help,
  main = main,
}

if cosmo.is_main() then
  os.exit(main(arg) or 0)
end

return home
