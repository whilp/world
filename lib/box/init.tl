-- box/init.tl: self-bootstrapping remote environment manager
local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local getopt = require("cosmo.getopt")
local spawn = require("cosmic.spawn")

local backend = require("box.backend")

local ENV_PATH <const> = "/zip/env.lua"

local record ParsedArgs
  backend_type: string  -- "sprite", "mac", or path to executable backend
  name: string          -- box name
  kind: string          -- backend kind (for local-run mode)
  cmd: string           -- "new", "run", "ssh", "zap", "list", or nil for all-in-one
  env_path: string      -- path to env.lua
  local_run: boolean    -- running remotely after upload
  repo: string          -- github repo (owner/name)
  release: string       -- release tag
  extra_args: {string}  -- extra args to pass to ssh
end

local function usage()
  io.stderr:write([[usage: box [options] [name] [command]

backends (pick one):
  --sprite              sprites.dev backend (bundled)
  --mac                 local mac backend (bundled)
  --backend <path>      executable backend

options:
  --env <path>          credentials file (default: ~/.config/box/env.lua)
  --repo <owner/name>   github repo (default: whilp/world)
  --release <tag>       release tag (default: latest)

commands:
  new                   create box only
  run                   bootstrap only
  ssh                   connect only
  scp <src> <dst>       copy files (: prefix = remote)
  zap                   destroy box
  list                  list boxes

If no command given, does new + run + ssh.

examples:
  box --sprite dev           # create, bootstrap, and ssh to sprite
  box --sprite dev new       # create sprite only
  box --sprite dev zap       # destroy sprite
  box --sprite dev scp file.txt :/tmp/file.txt   # upload
  box --sprite dev scp :/tmp/file.txt file.txt   # download
  box --backend my-backend dev   # use executable backend
  box --sprite --release 2026.01.16 dev
]])
end

local function parse_args(args: {string}): ParsedArgs
  local result: ParsedArgs = {
    backend_type = nil,
    name = nil,
    cmd = nil,
    env_path = path.join(os.getenv("HOME") or "", ".config", "box", "env.lua"),
    local_run = false,
    repo = nil,
    release = nil,
    extra_args = {},
  }

  local longopts = {
    {"sprite", "none"},
    {"mac", "none"},
    {"backend", "required"},
    {"env", "required"},
    {"repo", "required"},
    {"release", "required"},
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
    elseif opt == "repo" then
      result.repo = optarg
    elseif opt == "release" then
      result.release = optarg
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

local function load_backend(backend_type: string): backend.Backend, string
  if backend_type == "sprite" then
    return require("box.sprite") as backend.Backend
  elseif backend_type == "mac" then
    return require("box.mac") as backend.Backend
  elseif backend_type then
    -- All other backends are executables
    local generic = require("box.generic")
    return generic.load(backend_type)
  else
    return nil, "no backend specified (use --sprite, --mac, or --backend)"
  end
end

local function load_env(env_path: string, name: string): backend.Env, string
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
    ok, env = pcall(env as function(string): backend.Env, name)
    if not ok then
      return nil, "failed to call env function: " .. tostring(env)
    end
  end

  return env as backend.Env
end

local function cmd_new(be: backend.Backend, name: string): boolean, string
  io.stderr:write("==> creating " .. name .. "\n")

  -- Destroy existing box first (ignore errors)
  be.destroy(name)

  local result = be.new(name)
  if not result.ok then
    return false, result.err or "unknown error"
  end
  return true
end

local function cmd_run(be: backend.Backend, name: string, env_path: string, repo: string, release: string): boolean, string
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

  -- Load env.lua and add bootstrap config
  local env, err = load_env(env_path, name)
  if not env then
    return false, err
  end
  env.bootstrap = {
    repo = repo,
    release = release,
  }
  local env_data = "return " .. cosmo.EncodeLua(env)

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

  -- Run bootstrap on remote
  local cmd = "chmod +x /tmp/box && /tmp/box --local-run " .. name .. " " .. be.name
  result = be.exec(name, cmd)
  cleanup()
  if not result.ok then
    return false, "bootstrap failed: " .. (result.err or "unknown")
  end

  return true
end

local function cmd_ssh(be: backend.Backend, name: string, extra_args: {string}): boolean, string
  io.stderr:write("==> connecting to " .. name .. "\n")
  local result = be.ssh(name, table.unpack(extra_args))
  if not result.ok then
    return false, result.err or "unknown error"
  end
  return true
end

local function cmd_zap(be: backend.Backend, name: string): boolean, string
  io.stderr:write("==> destroying " .. name .. "\n")
  local result = be.destroy(name)
  if not result.ok then
    return false, result.err or "unknown error"
  end
  return true
end

local function cmd_list(be: backend.Backend): boolean, string
  local result = be.list()
  if not result.ok then
    return false, result.err or "unknown error"
  end
  return true
end

local function cmd_scp(be: backend.Backend, name: string, src: string, dst: string): boolean, string
  if not src or not dst then
    return false, "scp requires source and destination arguments"
  end

  local src_remote = src:sub(1, 1) == ":"
  local dst_remote = dst:sub(1, 1) == ":"

  if src_remote and dst_remote then
    return false, "cannot copy between two remote paths"
  end
  if not src_remote and not dst_remote then
    return false, "one of source or destination must be remote (prefix with :)"
  end

  local result: backend.Result
  if src_remote then
    -- download: remote -> local
    local remote_path = src:sub(2)
    io.stderr:write("==> downloading " .. remote_path .. " from " .. name .. "\n")
    result = be.download(name, remote_path, dst)
  else
    -- upload: local -> remote
    local remote_path = dst:sub(2)
    -- if dest is a directory (. or ends with /), append source basename
    if remote_path == "." or remote_path == "" or remote_path:sub(-1) == "/" then
      local basename = path.basename(src)
      if remote_path == "" then
        remote_path = basename
      else
        remote_path = path.join(remote_path, basename)
      end
    end
    io.stderr:write("==> uploading " .. src .. " to " .. name .. "\n")
    result = be.upload(name, src, remote_path)
  end

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
  local env: backend.Env
  if type(env_chunk) == "function" then
    ok, env = pcall(env_chunk as function(string): backend.Env, name)
    if not ok then
      return 1, "failed to call env function: " .. tostring(env)
    end
  else
    env = env_chunk as backend.Env
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

  local be: backend.Backend
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
    ok, err = cmd_run(be, parsed.name, parsed.env_path, parsed.repo, parsed.release)
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
  elseif parsed.cmd == "scp" then
    local src = parsed.extra_args[1]
    local dst = parsed.extra_args[2]
    ok, err = cmd_scp(be, parsed.name, src, dst)
    if not ok then return 1, err end
    return 0
  elseif parsed.cmd == nil then
    -- All-in-one: new + run + ssh (skip ssh if no tty)
    ok, err = cmd_new(be, parsed.name)
    if not ok then return 1, err end
    ok, err = cmd_run(be, parsed.name, parsed.env_path, parsed.repo, parsed.release)
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
