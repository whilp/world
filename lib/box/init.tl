-- box/init.tl: self-bootstrapping remote environment manager
local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local getopt = require("cosmo.getopt")
local spawn = require("cosmic.spawn")

local backend_mod = require("box.backend")

local record Backend
  name: string
  new: function(name: string, ...: string): backend_mod.Result
  ssh: function(name: string, ...: string): backend_mod.Result
  exec: function(name: string, cmd: string): backend_mod.Result
  upload: function(name: string, src: string, dst: string): backend_mod.Result
  destroy: function(name: string): backend_mod.Result
  list: function(): backend_mod.Result
end

local record ParsedArgs
  backend_type: string  -- "sprite", "mac", or path to backend file
  name: string          -- box name
  cmd: string           -- "new", "run", "ssh", "zap", "list", or nil for all-in-one
  env_path: string      -- path to env.lua
  local_run: boolean    -- running remotely after upload
  extra_args: {string}  -- extra args to pass to ssh
end

local function die(msg: string)
  io.stderr:write("error: " .. msg .. "\n")
  os.exit(1)
end

local function usage()
  io.stderr:write([[usage: box [options] [name] [command]

backends (pick one):
  --sprite              sprites.dev backend (bundled)
  --mac                 local mac backend (bundled)
  --backend <file>      custom backend (.tl or .lua)

options:
  --env <path>          credentials file (default: ~/.config/box/env.lua)

commands:
  new                   create box only
  run                   bootstrap only
  ssh                   connect only
  zap                   destroy box
  list                  list boxes

If no command given, does new + run + ssh.

examples:
  box --sprite dev           # create, bootstrap, and ssh to sprite
  box --sprite dev new       # create sprite only
  box --sprite dev zap       # destroy sprite
  box --backend pay.lua dev  # use custom backend
]])
end

local function parse_args(args: {string}): ParsedArgs
  local result: ParsedArgs = {
    backend_type = nil,
    name = nil,
    cmd = nil,
    env_path = path.join(os.getenv("HOME") or "", ".config", "box", "env.lua"),
    local_run = false,
    extra_args = {},
  }

  local longopts = {
    {"sprite", "none"},
    {"mac", "none"},
    {"backend", "required"},
    {"env", "required"},
    {"local-run", "none"},
    {"help", "none"},
  }

  local parser = getopt.new(args, "h", longopts)

  while true do
    local opt, optarg = parser:next()
    if not opt then break end

    if opt == "sprite" then
      result.backend_type = "sprite"
    elseif opt == "mac" then
      result.backend_type = "mac"
    elseif opt == "backend" then
      result.backend_type = optarg
    elseif opt == "env" then
      result.env_path = optarg
    elseif opt == "local-run" then
      result.local_run = true
    elseif opt == "h" or opt == "help" then
      usage()
      os.exit(0)
    elseif opt == "?" then
      die("invalid option")
    end
  end

  local remaining = parser:remaining()
  if remaining then
    for i, v in ipairs(remaining) do
      if i == 1 then
        -- First remaining arg is name or command
        if v == "list" then
          result.cmd = "list"
        else
          result.name = v
        end
      elseif i == 2 then
        result.cmd = v
      else
        table.insert(result.extra_args, v)
      end
    end
  end

  return result
end

local function load_backend(backend_type: string): Backend
  if backend_type == "sprite" then
    return require("box.sprite") as Backend
  elseif backend_type == "mac" then
    return require("box.mac") as Backend
  elseif backend_type then
    -- Custom backend file
    local ext = backend_type:match("%.([^.]+)$")
    if ext == "tl" then
      -- Compile teal to lua first
      local tl = require("tl")
      local result = tl.process(backend_type)
      if result.syntax_errors and #result.syntax_errors > 0 then
        die("teal syntax error in " .. backend_type)
      end
      if result.type_errors and #result.type_errors > 0 then
        for _, err in ipairs(result.type_errors) do
          io.stderr:write(backend_type .. ":" .. err.y .. ": " .. err.msg .. "\n")
        end
        die("teal type errors in " .. backend_type)
      end
      local chunk, err = load(tl.pretty_print_ast(result.ast), "@" .. backend_type)
      if not chunk then
        die("failed to load compiled teal: " .. (err or "unknown error"))
      end
      return chunk() as Backend
    else
      local chunk, err = loadfile(backend_type)
      if not chunk then
        die("failed to load backend: " .. (err or "unknown error"))
      end
      return chunk() as Backend
    end
  else
    die("no backend specified (use --sprite, --mac, or --backend)")
    return nil
  end
end

local function load_env(env_path: string, name: string): backend_mod.Env
  local st = unix.stat(env_path)
  if not st then
    die("env file not found: " .. env_path)
  end

  local chunk, err = loadfile(env_path)
  if not chunk then
    die("failed to load env: " .. (err or "unknown error"))
  end

  local ok, env = pcall(chunk)
  if not ok then
    die("failed to execute env: " .. tostring(env))
  end

  -- env.lua can be a function(name) or a table
  if type(env) == "function" then
    ok, env = pcall(env as function(string): backend_mod.Env, name)
    if not ok then
      die("failed to call env function: " .. tostring(env))
    end
  end

  return env as backend_mod.Env
end

local function cmd_new(be: Backend, name: string): boolean
  io.stderr:write("==> creating " .. name .. "\n")
  local result = be.new(name)
  if not result.ok then
    io.stderr:write("error: " .. (result.err or "unknown error") .. "\n")
    return false
  end
  return true
end

local function copy_file(src: string, dst: string, mode: number): boolean, string
  local src_f, err = io.open(src, "rb")
  if not src_f then
    return false, "failed to open source: " .. (err or "unknown")
  end
  local data = src_f:read("*a")
  src_f:close()

  local dst_f: FILE
  dst_f, err = io.open(dst, "wb")
  if not dst_f then
    return false, "failed to open destination: " .. (err or "unknown")
  end
  dst_f:write(data)
  dst_f:close()

  if mode then
    unix.chmod(dst, mode)
  end
  return true
end

local function cmd_run(be: Backend, name: string, env_path: string): boolean
  io.stderr:write("==> bootstrapping " .. name .. "\n")

  -- Get path to self (the box binary)
  -- In cosmopolitan lua, arg[-1] is the executable path
  local self_path = arg[-1]
  if not self_path then
    die("cannot determine path to self")
  end

  -- Resolve to absolute path
  if not self_path:match("^/") then
    local cwd = unix.getcwd()
    self_path = path.join(cwd, self_path)
  end

  -- Check if file exists
  local st = unix.stat(self_path)
  if not st then
    die("cannot find self at: " .. self_path)
  end

  -- Verify env.lua exists
  local env_src = io.open(env_path, "rb")
  if not env_src then
    die("failed to open env: " .. env_path)
  end
  env_src:close()

  local tmpdir = os.getenv("TMPDIR") or "/tmp"
  local pid = tostring(unix.getpid())

  -- Extract bundled zip tool to temp
  local tmp_zip = path.join(tmpdir, "box-zip." .. pid)
  local ok, err = copy_file("/zip/zip", tmp_zip, tonumber("755", 8))
  if not ok then
    die("failed to extract zip tool: " .. (err or "unknown"))
  end

  -- Copy box binary to temp
  local tmp_box = path.join(tmpdir, "box." .. pid)
  ok, err = copy_file(self_path, tmp_box, tonumber("755", 8))
  if not ok then
    unix.unlink(tmp_zip)
    die("failed to copy box: " .. (err or "unknown"))
  end

  -- Create temp dir for env.lua with correct name
  local env_dir = path.join(tmpdir, "box-env." .. pid)
  unix.makedirs(env_dir)
  local env_in_dir = path.join(env_dir, "env.lua")
  ok, err = copy_file(env_path, env_in_dir, tonumber("600", 8))
  if not ok then
    unix.unlink(tmp_zip)
    unix.unlink(tmp_box)
    unix.rmrf(env_dir)
    die("failed to copy env: " .. (err or "unknown"))
  end

  -- Zip env.lua into the box binary copy
  local handle = spawn({tmp_zip, "-qj", tmp_box, env_in_dir})
  if not handle then
    unix.unlink(tmp_zip)
    unix.unlink(tmp_box)
    unix.rmrf(env_dir)
    die("failed to run zip")
  end
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    unix.unlink(tmp_zip)
    unix.unlink(tmp_box)
    unix.rmrf(env_dir)
    die("zip failed: exit " .. tostring(exit_code))
  end

  -- Cleanup temp files (keep tmp_box for upload)
  unix.unlink(tmp_zip)
  unix.rmrf(env_dir)

  -- Upload modified box binary to remote
  local result = be.upload(name, tmp_box, "/tmp/box")
  unix.unlink(tmp_box)
  if not result.ok then
    io.stderr:write("error: upload failed: " .. (result.err or "unknown") .. "\n")
    return false
  end

  -- Run bootstrap on remote (pass name for env.lua function support)
  result = be.exec(name, "chmod +x /tmp/box && /tmp/box --local-run " .. name)
  if not result.ok then
    io.stderr:write("error: bootstrap failed: " .. (result.err or "unknown") .. "\n")
    return false
  end

  return true
end

local function cmd_ssh(be: Backend, name: string, extra_args: {string}): boolean
  io.stderr:write("==> connecting to " .. name .. "\n")
  local result = be.ssh(name, table.unpack(extra_args))
  if not result.ok then
    io.stderr:write("error: " .. (result.err or "unknown error") .. "\n")
    return false
  end
  return true
end

local function cmd_zap(be: Backend, name: string): boolean
  io.stderr:write("==> destroying " .. name .. "\n")
  local result = be.destroy(name)
  if not result.ok then
    io.stderr:write("error: " .. (result.err or "unknown error") .. "\n")
    return false
  end
  return true
end

local function cmd_list(be: Backend): boolean
  local result = be.list()
  if not result.ok then
    io.stderr:write("error: " .. (result.err or "unknown error") .. "\n")
    return false
  end
  return true
end

local function cmd_local_run(name: string): integer
  -- Running on remote after upload
  -- Load env from /zip/env.lua (zipped into self before upload)
  local ok, env_chunk = pcall(dofile, "/zip/env.lua")
  if not ok then
    io.stderr:write("error: failed to load /zip/env.lua: " .. tostring(env_chunk) .. "\n")
    return 1
  end

  -- env.lua can be a function(name) or a table
  local env: backend_mod.Env
  if type(env_chunk) == "function" then
    ok, env = pcall(env_chunk as function(string): backend_mod.Env, name)
    if not ok then
      io.stderr:write("error: failed to call env function: " .. tostring(env) .. "\n")
      return 1
    end
  else
    env = env_chunk as backend_mod.Env
  end

  local run = require("box.run")
  return run.bootstrap(env)
end

local function main(args: {string}): integer
  local parsed = parse_args(args)

  -- Handle local-run mode (running on remote)
  if parsed.local_run then
    return cmd_local_run(parsed.name or "")
  end

  -- Validate args
  if not parsed.backend_type then
    usage()
    return 1
  end

  local be = load_backend(parsed.backend_type)

  -- Handle list command (doesn't need name)
  if parsed.cmd == "list" then
    return cmd_list(be) and 0 or 1
  end

  -- All other commands need a name
  if not parsed.name then
    usage()
    return 1
  end

  -- Run specific command or all-in-one
  if parsed.cmd == "new" then
    return cmd_new(be, parsed.name) and 0 or 1
  elseif parsed.cmd == "run" then
    return cmd_run(be, parsed.name, parsed.env_path) and 0 or 1
  elseif parsed.cmd == "ssh" then
    return cmd_ssh(be, parsed.name, parsed.extra_args) and 0 or 1
  elseif parsed.cmd == "zap" then
    return cmd_zap(be, parsed.name) and 0 or 1
  elseif parsed.cmd == nil then
    -- All-in-one: new + run + ssh (skip ssh if no tty)
    if not cmd_new(be, parsed.name) then return 1 end
    if not cmd_run(be, parsed.name, parsed.env_path) then return 1 end
    if unix.isatty(0) then
      if not cmd_ssh(be, parsed.name, parsed.extra_args) then return 1 end
    end
    return 0
  else
    die("unknown command: " .. parsed.cmd)
    return 1
  end
end

if cosmo.is_main() then
  os.exit(main(arg) or 0)
end

return {
  parse_args = parse_args,
  load_backend = load_backend,
  load_env = load_env,
  main = main,
}
