local cosmo = require("cosmo")
local unix = cosmo.unix
local path = cosmo.path

local function debug_log(msg)
  if os.getenv("CLAUDE_WRAPPER_DEBUG") then
    io.stderr:write("claude wrapper: " .. msg .. "\n")
  end
end

local function find_claude_binary(paths)
  for i = 1, #paths do
    local p = paths[i]
    if p and unix.stat(p) then
      return p
    end
  end
  return nil
end

local function build_argv(append_prompts, mcp_config, user_args)
  local argv = {
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

local function scan_for_claude_deploy()
  local prefixes = {"/opt", "/usr", "/home"}
  for _, prefix in ipairs(prefixes) do
    local bin_path = path.join(prefix, "deploy", "claude-wrapper-hosts", "current", "bin", "claude")
    if unix.stat(bin_path) then
      return bin_path
    end
  end
  return nil
end

local function scan_for_atomic_install()
  local HOME = os.getenv("HOME")
  local share_dir = path.join(HOME, ".local", "share", "claude")

  if not unix.stat(share_dir) then
    return nil
  end

  local latest_path = nil
  local latest_version = nil

  for name, _ in unix.opendir(share_dir) do
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

  return latest_path
end

local function main(args)
  local HOME = os.getenv("HOME")

  local claude_paths = {
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

  local append_prompts = {}
  local env_append = os.getenv("CLAUDE_APPEND_SYSTEM_PROMPT")
  if env_append then
    table.insert(append_prompts, env_append)
  end

  local extras_mcp = path.join(HOME, "extras", "mcp.json")
  local argv = build_argv(append_prompts, extras_mcp, args)

  local execve_argv = {claude_bin}
  for i = 1, #argv do
    table.insert(execve_argv, argv[i])
  end

  debug_log("execve argv[0]: " .. execve_argv[1])
  debug_log("execve argv[1..]: " .. table.concat(argv, " "))

  local _, err = unix.execve(claude_bin, execve_argv, unix.environ())

  io.stderr:write("claude wrapper: failed to exec claude: " .. tostring(err) .. "\n")
  os.exit(1)
end

local claude = {
  find_claude_binary = find_claude_binary,
  build_argv = build_argv,
  scan_for_claude_deploy = scan_for_claude_deploy,
  scan_for_atomic_install = scan_for_atomic_install,
  main = main,
}

return claude
