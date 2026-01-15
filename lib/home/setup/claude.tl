local cosmo = require("cosmo")
local unix = require("cosmo.unix")
local path = require("cosmo.path")

local env_module = require("setup.env")

local record ClaudeVersionPlatform
	sha: string
end

local record ClaudeVersion
	version: string
	base_url: string
	url: string
	platforms: {string: ClaudeVersionPlatform}
end

local function load_version(): ClaudeVersion
	local ok, v = pcall(require, "claude.version") as (boolean, ClaudeVersion)
	if ok then
		return v
	end
	return nil
end

local function interpolate(template: string, vars: {string: string}): string
	return template:gsub("{([%w_]+)}", function(key: string): string
		return tostring(vars[key] or "")
	end)
end

local function run(env: env_module.Env): number
	if os.getenv("CODESPACES") == "true" then
		local version_info = load_version()
		if not version_info then
			io.stderr:write("error: failed to load claude version info\n")
			return 1
		end

		local platform = "linux-x64"
		local platform_info = version_info.platforms[platform]
		if not platform_info then
			io.stderr:write("error: no platform info for " .. platform .. "\n")
			return 1
		end

		local CLAUDE_VERSION = version_info.version
		local CLAUDE_SHA256 = platform_info.sha
		local CLAUDE_URL = interpolate(version_info.url, {
			base_url = version_info.base_url,
			version = CLAUDE_VERSION,
			platform = platform,
		})

		local short_sha = CLAUDE_SHA256:sub(1, 8)
		local version_dir = path.join(env.DST, ".local", "share", "claude", string.format("%s-%s", CLAUDE_VERSION, short_sha))
		local bin_dir = path.join(version_dir, "bin")
		local claude_bin = path.join(bin_dir, "claude")

		if unix.stat(claude_bin) then
			io.stderr:write("claude " .. CLAUDE_VERSION .. " already installed\n")
		else
			unix.makedirs(bin_dir)

			local status, _, body = cosmo.Fetch(CLAUDE_URL, {maxresponse = 300 * 1024 * 1024})
			if not status or status ~= 200 then
				io.stderr:write("error: failed to download claude binary\n")
				return 1
			end

			local actual_sha256 = cosmo.EncodeHex(cosmo.Sha256(body)):lower()
			if actual_sha256 ~= CLAUDE_SHA256 then
				io.stderr:write("error: claude binary checksum verification failed\n")
				return 1
			end

			local fd = unix.open(claude_bin, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("0755", 8))
			if not fd or fd < 0 then
				io.stderr:write("error: failed to create claude binary\n")
				return 1
			end
			unix.write(fd, body)
			unix.close(fd)
		end
	end

	local config = path.join(env.DST, ".claude.json")
	if unix.stat(config) then
		return 0
	end

	local claude_credentials = os.getenv("CLAUDE_CREDENTIALS")
	if claude_credentials then
		local creds_dir = path.join(env.DST, ".claude")
		unix.makedirs(creds_dir)
		local creds_file = path.join(creds_dir, ".credentials.json")
		local fd = unix.open(creds_file, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("0600", 8))
		if fd and fd >= 0 then
			unix.write(fd, claude_credentials)
			unix.close(fd)
		end
	end

	local settings = [[{
  "numStartups": 1,
  "installMethod": "unknown",
  "autoUpdates": false,
  "theme": "dark-daltonized",
  "hasCompletedOnboarding": true,
  "bypassPermissionsModeAccepted": true
}]]

	local fd = unix.open(config, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("0644", 8))
	if fd and fd >= 0 then
		unix.write(fd, settings)
		unix.close(fd)
	end

	return 0
end

return {
	run = run,
}
