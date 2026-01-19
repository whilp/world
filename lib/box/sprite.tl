-- box/sprite.tl: sprites.dev backend for box

local unix = require("cosmo.unix")
local spawn = require("cosmic.spawn")
local cosmo = require("cosmo")

local backend = require("box.backend")
local env = require("box.env")
local claude = require("box.claude")
local github = require("box.github")
local git = require("box.git")

local function ok(): backend.Result
  return {ok = true}
end

local function err(msg: string): backend.Result
  return {ok = false, err = msg}
end

local function new(name: string, ...: string): backend.Result
  local handle = spawn({"sprite", "create", name})
  if not handle then
    return err("sprite binary not found")
  end
  local exit_code = handle:wait()

  -- sprites.dev sometimes returns errors but still creates the sprite
  -- check if it exists
  local list_handle = spawn({"sprite", "list"})
  if list_handle then
    local list_ok, list_out = list_handle:read()
    if list_ok and list_out and (list_out as string):find(name, 1, true) then
      return ok()
    end
  end

  if exit_code ~= 0 then
    return err("failed to create sprite: exit " .. tostring(exit_code))
  end

  return ok()
end

local function ssh(name: string, ...: string): backend.Result
  local args = {"sprite", "console", "-s", name}
  for i = 1, select("#", ...) do
    local a = select(i, ...)
    table.insert(args, a)
  end

  unix.execvp("sprite", args)
  return err("failed to exec sprite console")
end

local function exec(name: string, cmd: string): backend.Result
  local handle = spawn({"sprite", "exec", "-s", name, "--", "bash", "-c", cmd}, {
    stdout = 1,  -- pass through to terminal
    stderr = 2,
  })
  if not handle then
    return err("sprite binary not found")
  end
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    return err("exec failed: exit " .. tostring(exit_code))
  end
  return ok()
end

local function upload(name: string, src: string, dst: string): backend.Result
  -- sprite -file doesn't handle relative paths correctly, resolve to absolute
  if not dst:match("^/") then
    local home_handle = spawn({"sprite", "exec", "-s", name, "--", "bash", "-c", "echo $HOME"})
    if not home_handle then
      return err("sprite binary not found")
    end
    local read_ok, home = home_handle:read()  -- read() calls wait() internally
    if not read_ok or not home then
      return err("failed to get remote home directory")
    end
    home = (home as string):gsub("%s+$", "")
    if dst == "." or dst == "./" then
      dst = home
    elseif dst:sub(1, 2) == "./" then
      dst = home .. dst:sub(2)
    else
      dst = home .. "/" .. dst
    end
  end

  local file_arg = src .. ":" .. dst
  local handle = spawn({"sprite", "exec", "-s", name, "-file", file_arg, "--", "true"})
  if not handle then
    return err("sprite binary not found")
  end
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    return err("upload failed: exit " .. tostring(exit_code))
  end
  return ok()
end

local function download(name: string, src: string, dst: string): backend.Result
  local handle = spawn({"sprite", "exec", "-s", name, "--", "cat", src})
  if not handle then
    return err("sprite binary not found")
  end
  local read_ok, content, exit_code = handle:read()  -- read() calls wait() internally
  if exit_code ~= 0 then
    return err("download failed: exit " .. tostring(exit_code))
  end
  if not read_ok or not content then
    return err("failed to read remote file")
  end
  if not cosmo.Barf(dst, content as string) then
    return err("failed to write local file: " .. dst)
  end
  return ok()
end

local function destroy(name: string): backend.Result
  local handle = spawn({"sprite", "destroy", "-s", name, "-force"})
  if not handle then
    return err("sprite binary not found")
  end
  local exit_code = handle:wait()
  if exit_code ~= 0 then
    return err("destroy failed: exit " .. tostring(exit_code))
  end
  return ok()
end

local function list(): backend.Result
  unix.execvp("sprite", {"sprite", "list"})
  return err("failed to exec sprite list")
end

local function env_vars_to_config(): backend.Env
  local config: backend.Env = {}

  -- Git configuration from GIT_NAME and GIT_EMAIL
  local git_name = os.getenv("GIT_NAME")
  local git_email = os.getenv("GIT_EMAIL")
  if git_name or git_email then
    config.git = {
      name = git_name,
      email = git_email,
    }
  end

  -- GitHub configuration from GITHUB_TOKEN
  local github_token = os.getenv("GITHUB_TOKEN")
  if github_token then
    config.github = {
      ["github.com"] = github_token,
    }
  end

  -- Claude configuration from CLAUDE_CREDENTIALS (JSON)
  local claude_creds = os.getenv("CLAUDE_CREDENTIALS")
  if claude_creds then
    local ok, creds = pcall(cosmo.DecodeJson, claude_creds)
    if ok then
      config.claude = {
        credentials = creds,
      }
    end
  end

  return config
end

local function bootstrap(name: string): backend.Result
  local cfg = env_vars_to_config()

  -- Install claude binary
  io.stderr:write("installing claude...\n")
  local install_ok, install_err = claude.install()
  if not install_ok then
    return err(install_err or "claude install failed")
  end

  -- Configure gh auth
  local cfg_ok, cfg_err = github.configure(cfg)
  if not cfg_ok then
    return err(cfg_err or "github config failed")
  end

  -- Configure git identity
  cfg_ok, cfg_err = git.configure(cfg)
  if not cfg_ok then
    return err(cfg_err or "git config failed")
  end

  -- Configure claude credentials
  cfg_ok, cfg_err = claude.configure(cfg)
  if not cfg_ok then
    return err(cfg_err or "claude config failed")
  end

  return ok()
end

return {
  name = "sprite",
  new = new,
  ssh = ssh,
  exec = exec,
  upload = upload,
  download = download,
  destroy = destroy,
  list = list,
  bootstrap = bootstrap,
} as backend.Backend
