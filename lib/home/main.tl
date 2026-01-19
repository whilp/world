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

local function serialize_value(val: any, indent?: string): string
  indent = indent or ""
  local t = type(val)

  if t == "string" then
    return string.format("%q", val as string)
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
    local max_index: integer = 0
    for k, _ in pairs(tbl) do
      if type(k) ~= "number" or (k as number) < 1 or k ~= math.floor(k as number) then
        is_array = false
        break
      end
      if (k as integer) > max_index then
        max_index = k as integer
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
  return "return " .. serialize_value(tbl, "") .. "\n"
end

-- Main manifest structure (new format: lists tools, not individual files)
local record Manifest
  version: string
  tools: {string}
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
  if not bytes_written then
    unix.close(fd)
    return false, "failed to write data"
  end

  if bytes_written ~= #data then
    unix.close(fd)
    return false, "failed to write data (wrote " .. tostring(bytes_written) .. " of " .. #data .. " bytes)"
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
  filter_input: string
  -- For testing: override default paths
  unzip_bin: string
  dotfiles_zip: string
  tools_dir: string  -- directory containing per-tool zips
end

local function cmd_unpack(dest: string, force: boolean, opts: UnpackOpts): integer
  opts = opts or {} as UnpackOpts
  local stderr = opts.stderr or io.stderr
  local stdout = opts.stdout or io.stdout
  local verbose = opts.verbose or false
  local dry_run = opts.dry_run or false

  if not dest then
    stderr:write("error: destination path required\n")
    stderr:write("usage: home unpack [--force] <destination>\n")
    return 1
  end

  -- Paths (can be overridden for testing)
  local unzip_src = opts.unzip_bin or "/zip/unzip"
  local dotfiles_src = opts.dotfiles_zip or "/zip/dotfiles.zip"
  local tools_src = opts.tools_dir or "/zip/tools"

  if dry_run then
    stdout:write("would extract dotfiles and tools to " .. dest .. "\n")
    return 0
  end

  if not unix.makedirs(dest) then
    stderr:write("error: failed to create destination directory\n")
    return 1
  end

  local spawn = require("cosmic.spawn")

  -- Extract unzip binary to temp location
  local tmpdir = os.getenv("TMPDIR") or "/tmp"
  local pid = unix.getpid() or 0
  local tmp_unzip = path.join(tmpdir, "unzip." .. tostring(pid))

  -- Copy unzip to temp (may already be a real path for testing)
  local unzip_stat = unix.stat(unzip_src)
  if not unzip_stat then
    stderr:write("error: unzip not found at " .. unzip_src .. "\n")
    return 1
  end

  local ok: boolean
  local err: string
  if unzip_src:match("^/zip/") then
    ok, err = copy_file(unzip_src, tmp_unzip, tonumber("0755", 8), true)
    if not ok then
      stderr:write("error: failed to extract unzip: " .. (err or "unknown error") .. "\n")
      return 1
    end
  else
    tmp_unzip = unzip_src  -- Use provided path directly
  end

  -- Helper to extract a zip file
  local function extract_zip(zip_src: string, zip_name: string, required: boolean): boolean
    local tmp_zip = path.join(tmpdir, zip_name .. "." .. tostring(pid) .. ".zip")

    local zip_stat = unix.stat(zip_src)
    if not zip_stat then
      if required then
        stderr:write("error: " .. zip_name .. " not found at " .. zip_src .. "\n")
        return false
      end
      if verbose then
        stderr:write("warning: " .. zip_name .. " not found at " .. zip_src .. "\n")
      end
      return true  -- Not an error, just skip
    end

    -- Copy zip to temp if from /zip/
    local actual_zip = zip_src
    if zip_src:match("^/zip/") then
      local copy_ok, copy_err = copy_file(zip_src, tmp_zip, tonumber("0644", 8), true)
      if not copy_ok then
        stderr:write("error: failed to extract " .. zip_name .. ": " .. (copy_err or "unknown error") .. "\n")
        return false
      end
      actual_zip = tmp_zip
    end

    -- Run unzip
    local unzip_args: {string} = {tmp_unzip, "-q", actual_zip, "-d", dest}
    if force then
      table.insert(unzip_args, 2, "-o")
    end
    local result = spawn(unzip_args):wait()

    -- Cleanup temp zip
    if zip_src:match("^/zip/") then unix.unlink(tmp_zip) end

    if result ~= 0 then
      stderr:write("error: unzip " .. zip_name .. " failed with exit code " .. tostring(result) .. "\n")
      return false
    end

    return true
  end

  -- Extract dotfiles.zip (required)
  if not extract_zip(dotfiles_src, "dotfiles", true) then
    if unzip_src:match("^/zip/") then unix.unlink(tmp_unzip) end
    return 1
  end

  -- Extract each tool zip from tools_dir
  local tools_stat = unix.stat(tools_src)
  if tools_stat and unix.S_ISDIR(tools_stat:mode()) then
    local tools_dir_iter = unix.opendir(tools_src)
    if tools_dir_iter then
      for entry in tools_dir_iter do
        if entry ~= "." and entry ~= ".." and entry:match("%.zip$") then
          local tool_name = entry:gsub("%.zip$", "")
          local tool_zip = path.join(tools_src, entry)
          if not extract_zip(tool_zip, tool_name, false) then
            if unzip_src:match("^/zip/") then unix.unlink(tmp_unzip) end
            return 1
          end
          if verbose then
            stdout:write("extracted " .. tool_name .. "\n")
          end
        end
      end
    end
  end

  if unzip_src:match("^/zip/") then unix.unlink(tmp_unzip) end

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
  stderr:write("  unpack [options] <dest>  extract files to destination\n")
  stderr:write("  setup <dest>             run setup scripts\n")
  stderr:write("  mac [script]             run macOS defaults scripts\n")
  stderr:write("  version                  show build version\n")
  stderr:write("\nunpack options:\n")
  stderr:write("  --force, -f              overwrite existing files\n")
  stderr:write("  --verbose, -v            show files as extracted\n")
  stderr:write("  --dry-run, -n            show what would be extracted\n")
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
  parse_args: function(args: {string}): ParsedArgs
  cmd_unpack: function(dest: string, force: boolean, opts: UnpackOpts): integer
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
  parse_args = parse_args,
  cmd_unpack = cmd_unpack,
  cmd_setup = cmd_setup,
  cmd_mac = cmd_mac,
  cmd_version = cmd_version,
  cmd_help = cmd_help,
  main = main,
}

if cosmo.is_main() then
  os.exit(main(arg, {} as MainOpts) or 0)
end

return home
