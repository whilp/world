local unix = require("cosmo.unix")
local path = require("cosmo.path")

local record Stat
  mode: function(self): number
  size: function(self): number
  mtim: function(self): number
end

local record DirHandle
  read: function(self): string
  close: function(self)
end

local function debug_log(msg: string)
  if os.getenv("CLAUDE_WRAPPER_DEBUG") then
    io.stderr:write("claude wrapper: " .. msg .. "\n")
  end
end

local function find_claude_binary(paths: {string}): string
  for i = 1, #paths do
    local p = paths[i]
    if p and unix.stat(p) then
      return p
    end
  end
  return nil
end

local function build_argv(append_prompts: {string}, mcp_config: string, user_args: {string}): {string}
  local argv: {string} = {
    "--dangerously-skip-permissions",
    "--strict-mcp-config",
  }

  if #append_prompts > 0 then
    table.insert(argv, "--append-system-prompt")
    table.insert(argv, table.concat(append_prompts, "\n\n"))
  end

  if mcp_config and unix.stat(mcp_config) then
    table.insert(argv, "--mcp-config")
    table.insert(argv, mcp_config)
  end

  for i = 1, #user_args do
    table.insert(argv, user_args[i])
  end

  return argv
end

local function scan_for_claude_deploy(): string
  -- TODO: use glob("/*/deploy/...") when cosmic-lua has glob support
  local handle = unix.opendir("/") as DirHandle
  if not handle then return nil end

  while true do
    local name = handle:read()
    if not name then break end
    if name ~= "." and name ~= ".." then
      local bin_path = path.join("/", name, "deploy", "claude-wrapper-hosts", "current", "bin", "claude")
      if unix.stat(bin_path) then
        handle:close()
        return bin_path
      end
    end
  end
  handle:close()
  return nil
end

local function scan_for_atomic_install(): string
  local HOME = os.getenv("HOME")
  local share_dir = path.join(HOME, ".local", "share", "claude")

  local handle = unix.opendir(share_dir) as DirHandle
  if not handle then
    return nil
  end

  local latest_path: string = nil
  local latest_version: string = nil

  while true do
    local name = handle:read()
    if not name then break end
    if name ~= "." and name ~= ".." then
      local version = name:match("^([%d%.]+)%-")
      if version then
        local bin_path = path.join(share_dir, name, "claude")
        if unix.stat(bin_path) then
          if not latest_version or version > latest_version then
            latest_version = version
            latest_path = bin_path
          end
        end
      end
    end
  end
  handle:close()

  return latest_path
end

local function main(args: {string})
  local HOME = os.getenv("HOME")

  local claude_paths: {string} = {
    scan_for_claude_deploy(),
    scan_for_atomic_install(),
    path.join(HOME, ".local", "share", "claude", "bin", "claude"),
    "/usr/local/bin/claude",
  }

  local claude_bin = find_claude_binary(claude_paths)
  if not claude_bin then
    io.stderr:write("claude wrapper: no claude binary found in configured paths\n")
    os.exit(1)
  end

  debug_log("using binary at " .. claude_bin)

  local append_prompts: {string} = {}
  local env_append = os.getenv("CLAUDE_APPEND_SYSTEM_PROMPT")
  if env_append then
    table.insert(append_prompts, env_append)
  end

  local extras_mcp = path.join(HOME, "extras", "mcp.json")
  local argv = build_argv(append_prompts, extras_mcp, args)

  local execve_argv: {string} = {claude_bin}
  for i = 1, #argv do
    table.insert(execve_argv, argv[i])
  end

  debug_log("execve argv[0]: " .. execve_argv[1])
  debug_log("execve argv[1..]: " .. table.concat(argv, " "))

  local _, err = unix.execve(claude_bin, execve_argv, unix.environ())

  io.stderr:write("claude wrapper: failed to exec claude: " .. tostring(err) .. "\n")
  os.exit(1)
end

local record claude
  find_claude_binary: function(paths: {string}): string
  build_argv: function(append_prompts: {string}, mcp_config: string, user_args: {string}): {string}
  scan_for_claude_deploy: function(): string
  scan_for_atomic_install: function(): string
  main: function(args: {string})
end

local M: claude = {
  find_claude_binary = find_claude_binary,
  build_argv = build_argv,
  scan_for_claude_deploy = scan_for_claude_deploy,
  scan_for_atomic_install = scan_for_atomic_install,
  main = main,
}

return M
