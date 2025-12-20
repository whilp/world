local cosmo = require("cosmo")
local unix = cosmo.unix

local function find_claude_binary(paths)
  for _, path in ipairs(paths) do
    if unix.stat(path) then
      return path
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
    local path = prefix .. "/deploy/claude-wrapper-hosts/current/bin/claude"
    if unix.stat(path) then
      return path
    end
  end
  return nil
end

local function main(args)
  local HOME = os.getenv("HOME")

  local claude_paths = {
    scan_for_claude_deploy(),
    HOME .. "/.local/share/claude/bin/claude",
    "/usr/local/bin/claude",
  }

  local claude_bin = find_claude_binary(claude_paths)
  if not claude_bin then
    io.stderr:write("claude wrapper: no claude binary found in configured paths\n")
    os.exit(1)
  end

  local append_prompts = {}
  local env_append = os.getenv("CLAUDE_APPEND_SYSTEM_PROMPT")
  if env_append then
    table.insert(append_prompts, env_append)
  end

  local extras_mcp = HOME .. "/extras/mcp.json"
  local argv = build_argv(append_prompts, extras_mcp, args)

  local result, err = unix.execve(claude_bin, argv, unix.environ())

  io.stderr:write("claude wrapper: failed to exec claude: " .. tostring(err) .. "\n")
  os.exit(1)
end

local claude = {
  find_claude_binary = find_claude_binary,
  build_argv = build_argv,
  scan_for_claude_deploy = scan_for_claude_deploy,
  main = main,
}

return claude
