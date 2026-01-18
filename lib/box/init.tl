-- box/init.tl: self-bootstrapping remote environment manager
local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local getopt = require("cosmo.getopt")
local spawn = require("cosmic.spawn")

local backend_mod = require("box.backend")

local ENV_PATH <const> = "/zip/env.lua"

local record ParsedArgs
  backend_type: string  -- "sprite", "mac", or path to backend file
  name: string          -- box name
  kind: string          -- backend kind (for local-run mode)
  cmd: string           -- "new", "run", "ssh", "zap", "list", or nil for all-in-one
  env_path: string      -- path to env.lua
  local_run: boolean    -- running remotely after upload
  extra_args: {string}  -- extra args to pass to ssh
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
      return nil, "invalid option"
    end
  end

  local remaining = parser:remaining()
  if remaining then
    for i, v in ipairs(remaining) do
      if i == 1 then
        if v == "list" then
          result.cmd = "list"
        else
          result.name = v
        end
      elseif i == 2 then
        if result.local_run then
          result.kind = v
        else
          result.cmd = v
        end
      else
        table.insert(result.extra_args, v)
      end
    end
  end

  return result
end

local function load_teal_backend(filepath: string): backend_mod.Backend, string
  local tl = require("tl")
  local result = tl.process(filepath)
  if result.syntax_errors and #result.syntax_errors > 0 then
    return nil, "teal syntax error in " .. filepath
  end
  if result.type_errors and #result.type_errors > 0 then
    for _, err in ipairs(result.type_errors) do
      io.stderr:write(filepath .. ":" .. err.y .. ": " .. err.msg .. "\n")
    end
    return nil, "teal type errors in " .. filepath
  end
  local chunk, err = load(tl.pretty_print_ast(result.ast), "@" .. filepath)
  if not chunk then
    return nil, "failed to load compiled teal: " .. (err or "unknown error")
  end
  return chunk() as backend_mod.Backend
end

local function load_lua_backend(filepath: string): backend_mod.Backend, string
  local chunk, err = loadfile(filepath)
  if not chunk then
    return nil, "failed to load backend: " .. (err or "unknown error")
  end
  return chunk() as backend_mod.Backend
end

local function load_backend(backend_type: string): backend_mod.Backend, string
  if backend_type == "sprite" then
    return require("box.sprite") as backend_mod.Backend
  elseif backend_type == "mac" then
    return require("box.mac") as backend_mod.Backend
  elseif backend_type then
    local ext = backend_type:match("%.([^.]+)$")
    if ext == "tl" then
      return load_teal_backend(backend_type)
    else
      return load_lua_backend(backend_type)
    end
  else
    return nil, "no backend specified (use --sprite, --mac, or --backend)"
  end
end

local function load_env(env_path: string, name: string): backend_mod.Env, string
  local content = cosmo.Slurp(env_path)
  if not content then
    return nil, "env file not found: " .. env_path
  end

  local chunk, err = load(content, "@" .. env_path)
  if not chunk then
    return nil, "failed to load env: " .. (err or "unknown error")
  end

  local ok, env = pcall(chunk)
  if not ok then
    return nil, "failed to execute env: " .. tostring(env)
  end

  -- env.lua can be a function(name) or a table
  if type(env) == "function" then
    ok, env = pcall(env as function(string): backend_mod.Env, name)
    if not ok then
      return nil, "failed to call env function: " .. tostring(env)
    end
  end

  return env as backend_mod.Env
end

local function cmd_new(be: backend_mod.Backend, name: string): boolean, string
  io.stderr:write("==> creating " .. name .. "\n")

  -- Destroy existing box first (ignore errors)
  be.destroy(name)

  local result = be.new(name)
  if not result.ok then
    return false, result.err or "unknown error"
  end
  return true
end

local function cmd_run(be: backend_mod.Backend, name: string, env_path: string): boolean, string
  io.stderr:write("==> bootstrapping " .. name .. "\n")

  -- Get path to self (the box binary)
  local self_path = arg[-1]
  if not self_path then
    return false, "cannot determine path to self"
  end

  -- Resolve to absolute path
  if not self_path:match("^/") then
    local cwd = unix.getcwd()
    self_path = path.join(cwd, self_path)
  end

  -- Check if file exists
  local self_data = cosmo.Slurp(self_path)
  if not self_data then
    return false, "cannot find self at: " .. self_path
  end

  -- Verify env.lua exists
  local env_data = cosmo.Slurp(env_path)
  if not env_data then
    return false, "failed to open env: " .. env_path
  end

  -- Create temp directory for work
  local tmpdir = unix.mkdtemp("/tmp/box.XXXXXX")
  if not tmpdir then
    return false, "failed to create temp directory"
  end

  local function cleanup()
    unix.rmrf(tmpdir)
  end

  -- Extract bundled zip tool to temp
  local zip_data = cosmo.Slurp("/zip/zip")
  if not zip_data then
    cleanup()
    return false, "failed to read bundled zip tool"
  end
  local tmp_zip = path.join(tmpdir, "zip")
  if not cosmo.Barf(tmp_zip, zip_data, tonumber("755", 8)) then
    cleanup()
    return false, "failed to extract zip tool"
  end

  -- Copy box binary to temp
  local tmp_box = path.join(tmpdir, "box")
  if not cosmo.Barf(tmp_box, self_data, tonumber("755", 8)) then
    cleanup()
    return false, "failed to copy box"
  end

  -- Write env.lua to temp dir
  local env_in_dir = path.join(tmpdir, "env.lua")
  if not cosmo.Barf(env_in_dir, env_data, tonumber("600", 8)) then
    cleanup()
    return false, "failed to copy env"
  end

  -- Zip env.lua into the box binary copy
  local handle = spawn({tmp_zip, "-qj", tmp_box, env_in_dir})
  if not handle then
    cleanup()
    return false, "failed to run zip"
  end
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    cleanup()
    return false, "zip failed: exit " .. tostring(exit_code)
  end

  -- Upload modified box binary to remote
  local result = be.upload(name, tmp_box, "/tmp/box")
  if not result.ok then
    cleanup()
    return false, "upload failed: " .. (result.err or "unknown")
  end

  -- Run bootstrap on remote (pass name and kind for env.lua function support)
  result = be.exec(name, "chmod +x /tmp/box && /tmp/box --local-run " .. name .. " " .. be.name)
  cleanup()
  if not result.ok then
    return false, "bootstrap failed: " .. (result.err or "unknown")
  end

  return true
end

local function cmd_ssh(be: backend_mod.Backend, name: string, extra_args: {string}): boolean, string
  io.stderr:write("==> connecting to " .. name .. "\n")
  local result = be.ssh(name, table.unpack(extra_args))
  if not result.ok then
    return false, result.err or "unknown error"
  end
  return true
end

local function cmd_zap(be: backend_mod.Backend, name: string): boolean, string
  io.stderr:write("==> destroying " .. name .. "\n")
  local result = be.destroy(name)
  if not result.ok then
    return false, result.err or "unknown error"
  end
  return true
end

local function cmd_list(be: backend_mod.Backend): boolean, string
  local result = be.list()
  if not result.ok then
    return false, result.err or "unknown error"
  end
  return true
end

local function cmd_local_run(name: string, kind: string): integer, string
  -- Running on remote after upload
  -- Load env from /zip/env.lua (zipped into self before upload)
  local ok, env_chunk = pcall(dofile, ENV_PATH)
  if not ok then
    return 1, "failed to load " .. ENV_PATH .. ": " .. tostring(env_chunk)
  end

  -- env.lua can be a function(name) or a table
  local env: backend_mod.Env
  if type(env_chunk) == "function" then
    ok, env = pcall(env_chunk as function(string): backend_mod.Env, name)
    if not ok then
      return 1, "failed to call env function: " .. tostring(env)
    end
  else
    env = env_chunk as backend_mod.Env
  end

  local run = require("box.run")
  return run.bootstrap(env, name, kind)
end

local function main(args: {string}): integer, string
  local parsed, err = parse_args(args)
  if not parsed then
    return 1, err
  end

  -- Handle local-run mode (running on remote)
  if parsed.local_run then
    return cmd_local_run(parsed.name or "", parsed.kind or "")
  end

  -- Validate args
  if not parsed.backend_type then
    usage()
    return 1
  end

  local be: backend_mod.Backend
  be, err = load_backend(parsed.backend_type)
  if not be then
    return 1, err
  end

  -- Handle list command (doesn't need name)
  if parsed.cmd == "list" then
    local ok: boolean
    ok, err = cmd_list(be)
    if not ok then
      return 1, err
    end
    return 0
  end

  -- All other commands need a name
  if not parsed.name then
    usage()
    return 1
  end

  -- Run specific command or all-in-one
  local ok: boolean
  if parsed.cmd == "new" then
    ok, err = cmd_new(be, parsed.name)
    if not ok then return 1, err end
    return 0
  elseif parsed.cmd == "run" then
    ok, err = cmd_run(be, parsed.name, parsed.env_path)
    if not ok then return 1, err end
    return 0
  elseif parsed.cmd == "ssh" then
    ok, err = cmd_ssh(be, parsed.name, parsed.extra_args)
    if not ok then return 1, err end
    return 0
  elseif parsed.cmd == "zap" then
    ok, err = cmd_zap(be, parsed.name)
    if not ok then return 1, err end
    return 0
  elseif parsed.cmd == nil then
    -- All-in-one: new + run + ssh (skip ssh if no tty)
    ok, err = cmd_new(be, parsed.name)
    if not ok then return 1, err end
    ok, err = cmd_run(be, parsed.name, parsed.env_path)
    if not ok then return 1, err end
    if unix.isatty(0) then
      ok, err = cmd_ssh(be, parsed.name, parsed.extra_args)
      if not ok then return 1, err end
    end
    return 0
  else
    return 1, "unknown command: " .. parsed.cmd
  end
end

if cosmo.is_main() then
  local code, err = main(arg)
  if err then
    io.stderr:write("error: " .. err .. "\n")
  end
  os.exit(code)
end

return {
  parse_args = parse_args,
  load_backend = load_backend,
  load_env = load_env,
  main = main,
}
