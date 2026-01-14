-- home binary: manages dotfiles extraction
local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local getopt = require("cosmo.getopt")

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

-- Manifest file info
local record FileInfo
  mode: number
end

-- Main manifest structure
local record Manifest
  version: string
  files: {string:FileInfo}
end

local function load_manifest(): Manifest
  local ok, manifest = pcall(dofile, "/zip/manifest.lua")
  if ok then
    return manifest as Manifest
  end
  return nil
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
    dest = nil,
  }

  if #args < 2 then
    return result
  end

  local cmd_args: {string} = {}
  for i = 2, #args do
    table.insert(cmd_args, args[i])
  end

  local longopts = {
    {"force", "none"},
    {"verbose", "none"},
    {"dry-run", "none"},
    {"only", "none"},
    {"null", "none"},
  }
  local parser = getopt.new(cmd_args, "fvn0", longopts)

  while true do
    local opt, _ = parser:next()
    if not opt then break end

    if opt == "f" or opt == "force" then
      result.force = true
    elseif opt == "v" or opt == "verbose" then
      result.verbose = true
    elseif opt == "n" or opt == "dry-run" then
      result.dry_run = true
    elseif opt == "only" then
      result.only = true
    elseif opt == "0" or opt == "null" then
      result.null = true
    elseif opt == "?" then
      io.stderr:write("error: invalid option\n")
      os.exit(1)
    end
  end

  local remaining = parser:remaining()
  if remaining and #remaining > 0 then
    if result.cmd == "mac" and not remaining[1]:match("^%-") then
      result.subcmd = remaining[1]
      if #remaining > 1 then
        result.dest = remaining[2]
      end
    else
      result.dest = remaining[1]
    end
  end

  return result
end

-- Options for cmd_unpack
local record UnpackOpts
  stderr: FILE
  stdout: FILE
  verbose: boolean
  dry_run: boolean
  only: boolean
  null: boolean
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
  local zip_root = opts.zip_root or "/zip/home/"

  if not dest then
    stderr:write("error: destination path required\n")
    stderr:write("usage: home unpack [--force] <destination>\n")
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

  -- Extract module zips from /zip/zips/ to .local/share
  if not dry_run then
    local zips_dir = "/zip/zips"
    local zips_stat = unix.stat(zips_dir)
    if zips_stat and unix.S_ISDIR(zips_stat:mode()) then
      local zips_list = unix.opendir(zips_dir)
      if zips_list then
        for zip_file in zips_list do
          if zip_file ~= "." and zip_file ~= ".." and zip_file:match("%.zip$") then
            local zip_path = path.join(zips_dir, zip_file)
            local share_dest = path.join(dest, ".local", "share")
            unix.makedirs(share_dest)

            -- Read zip from /zip filesystem to temp file
            local zip_data = read_file(zip_path)
            if zip_data then
              local temp_zip = unix.mkstemp("/tmp/module_XXXXXX.zip")
              if temp_zip then
                local temp_fd, temp_path = temp_zip as (number, string)
                unix.write(temp_fd, zip_data)
                unix.close(temp_fd)

                -- Extract the temp zip file
                local spawn = require("cosmic.spawn")
                local unzip_bin = path.join(dest, ".local", "bin", "unzip")
                -- Fall back to system unzip if not available yet
                if not unix.stat(unzip_bin) then
                  unzip_bin = "/usr/bin/unzip"
                end
                local handle = spawn({unzip_bin, "-o", "-q", "-d", share_dest, temp_path})
                local exit_code = handle:wait()

                -- Clean up temp file
                unix.unlink(temp_path)

                if exit_code ~= 0 and verbose then
                  stderr:write("warning: failed to extract " .. zip_file .. "\n")
                end
              end
            end
          end
        end
      end
    end
  end

  -- Create symlinks from tool root to versioned subdirectories
  if not dry_run then
    local version_mod = require("version")
    local share_dir = path.join(dest, ".local", "share")
    local share_stat = unix.stat(share_dir)
    if share_stat and unix.S_ISDIR(share_stat:mode()) then
      local tools_dir = unix.opendir(share_dir)
      if tools_dir then
        for tool_name in tools_dir do
          if tool_name ~= "." and tool_name ~= ".." then
            local tool_path = path.join(share_dir, tool_name)
            local tool_stat = unix.stat(tool_path)
            if tool_stat and unix.S_ISDIR(tool_stat:mode()) then
              -- Find versioned directory (contains version-sha pattern)
              local versioned_dir: string = nil
              local entries_dir = unix.opendir(tool_path)
              if entries_dir then
                for entry in entries_dir do
                  if entry ~= "." and entry ~= ".." and version_mod.is_version_dir(entry) then
                    versioned_dir = entry
                    break
                  end
                end
              end

              -- Create symlinks from tool root to versioned subdirectories
              if versioned_dir then
                local versioned_path = path.join(tool_path, versioned_dir)
                local versioned_entries = unix.opendir(versioned_path)
                if versioned_entries then
                  for item in versioned_entries do
                    if item ~= "." and item ~= ".." then
                      local link_target = path.join(versioned_dir, item)
                      local link_path = path.join(tool_path, item)
                      -- Only create symlink if it doesn't already exist
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
    table.insert(all_paths, { path = p, mode = info.mode })
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

  stderr:write("usage: home <command> [options]\n")
  stderr:write("\ncommands:\n")
  stderr:write("  list [options]           list embedded files\n")
  stderr:write("  unpack [options] <dest>  extract files to destination\n")
  stderr:write("  setup <dest>             run setup scripts\n")
  stderr:write("  mac [script]             run macOS defaults scripts\n")
  stderr:write("  version                  show build version\n")
  stderr:write("\nlist options:\n")
  stderr:write("  --verbose, -v            show permissions\n")
  stderr:write("  --null, -0               use null delimiter\n")
  stderr:write("\nunpack options:\n")
  stderr:write("  --force, -f              overwrite existing files\n")
  stderr:write("  --verbose, -v            show files as extracted\n")
  stderr:write("  --dry-run, -n            show what would be extracted\n")
  stderr:write("  --only                   only extract files listed on stdin\n")
  stderr:write("  --null, -0               read null-delimited paths (with --only)\n")
  stderr:write("\nmac subcommands:\n")
  stderr:write("  mac                      run all macOS defaults scripts\n")
  stderr:write("  mac list                 list available scripts\n")
  stderr:write("  mac <script>             run a specific script\n")
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

-- Module record
local record Home
  read_file: function(filepath: string): string, string
  serialize_value: function(val: any, indent: string): string
  serialize_table: function(tbl: {any:any}): string
  load_manifest: function(): Manifest
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
  read_file = read_file,
  serialize_value = serialize_value,
  serialize_table = serialize_table,
  load_manifest = load_manifest,
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
