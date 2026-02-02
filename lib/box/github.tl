-- box/github.tl: configure gh auth
--
-- Discovers GitHub hosts from environment variables:
--   GH_<NAME>_HOSTNAME - the hostname (e.g., github.com)
--   GH_<NAME>_TOKEN - the token for that host
--
-- Example:
--   GH_GITHUB_HOSTNAME=github.com
--   GH_GITHUB_TOKEN=ghp_xxx
--   GH_CORP_HOSTNAME=git.corp.example.com
--   GH_CORP_TOKEN=ghp_yyy

local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local record GitHubHost
  name: string
  hostname: string
  token: string
end

local function discover_hosts(): {GitHubHost}
  local hosts_by_name: {string:GitHubHost} = {}

  for _, entry in ipairs(unix.environ()) do
    local key, value = entry:match("^([^=]+)=(.*)$")
    if key then
      local name = key:match("^GH_(.+)_HOSTNAME$")
      if name then
        if not hosts_by_name[name] then
          hosts_by_name[name] = {name = name}
        end
        hosts_by_name[name].hostname = value
      end

      name = key:match("^GH_(.+)_TOKEN$")
      if name then
        if not hosts_by_name[name] then
          hosts_by_name[name] = {name = name}
        end
        hosts_by_name[name].token = value
      end
    end
  end

  -- Filter to only hosts with both hostname and token
  local hosts: {GitHubHost} = {}
  for _, host in pairs(hosts_by_name) do
    if host.hostname and host.token then
      table.insert(hosts, host)
    end
  end

  return hosts
end

local function auth_host(gh_bin: string, host: GitHubHost): boolean
  io.stderr:write("configuring gh auth for " .. host.hostname .. "...\n")

  local args: {string} = {gh_bin, "auth", "login", "-h", host.hostname, "--with-token"}
  local handle = spawn(args, {env = unix.environ()})

  if handle.stdin then
    handle.stdin:write(host.token .. "\n")
    handle.stdin:close()
  end

  local exit_code = handle:wait()
  if exit_code ~= 0 then
    io.stderr:write("warning: gh auth login failed for " .. host.hostname .. " (exit " .. tostring(exit_code) .. ")\n")
    return false
  end

  return true
end

local function configure(hostnames?: {string}): boolean, string
  local hosts = discover_hosts()

  -- Filter to specific hostnames if provided
  if hostnames then
    local filtered: {GitHubHost} = {}
    for _, host in ipairs(hosts) do
      for _, allowed in ipairs(hostnames) do
        if host.hostname == allowed then
          table.insert(filtered, host)
          break
        end
      end
    end
    hosts = filtered
  end

  if #hosts == 0 then
    return true -- no matching github config
  end

  local home = os.getenv("HOME")
  if not home then
    return false, "HOME not set"
  end

  local gh_bin = path.join(home, ".local", "share", "gh", "bin", "gh")
  local st = unix.stat(gh_bin)
  if not st then
    io.stderr:write("warning: gh not found at " .. gh_bin .. "\n")
    return true -- not an error, just skip
  end

  for _, host in ipairs(hosts) do
    auth_host(gh_bin, host)
  end

  return true
end

return {
  configure = configure,
}
