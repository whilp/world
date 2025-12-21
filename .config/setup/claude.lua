local cosmo = require("cosmo")
local unix = cosmo.unix
local util = require("util")

local function get_latest_version()
	local handle = io.popen("gh api repos/anthropics/claude-code/releases/latest --jq '.tag_name'", "r")
	if not handle then
		io.stderr:write("error: failed to fetch latest version\n")
		return nil
	end
	local version = handle:read("*l")
	handle:close()
	if version then
		version = version:match("^v?(.+)$")
	end
	return version
end

local function get_sha256(url)
	local temp_file = "/tmp/claude-download-" .. os.time()
	local ok = pcall(util.spawn, {"curl", "-fsSL", "-o", temp_file, url})
	if not ok then
		io.stderr:write("error: failed to download claude binary\n")
		return nil
	end

	local handle = io.popen(string.format("shasum -a 256 '%s'", temp_file), "r")
	local sha256
	if handle then
		local output = handle:read("*l")
		handle:close()
		if output then
			sha256 = output:match("^(%x+)")
		end
	end

	unix.unlink(temp_file)
	return sha256
end

local function print_latest()
	local version = get_latest_version()
	if not version then
		return 1
	end

	local url = string.format(
		"https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases/%s/linux-x64/claude",
		version
	)

	io.write("Fetching SHA256 for version " .. version .. "...\n")
	local sha256 = get_sha256(url)
	if not sha256 then
		return 1
	end

	io.write("\nLatest Claude Code version:\n")
	io.write("  Version: " .. version .. "\n")
	io.write("  SHA256:  " .. sha256 .. "\n")
	io.write("  URL:     " .. url .. "\n")
	return 0
end

local function run(env)
	if os.getenv("CODESPACES") == "true" then
		local CLAUDE_VERSION = "2.0.74"
		local CLAUDE_SHA256 = "43065ff86a1b952225e42042bf4dfe9f6b72ff8ed91686a23add5396b1a11e80"
		local CLAUDE_URL = string.format(
			"https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases/%s/linux-x64/claude",
			CLAUDE_VERSION
		)

		local short_sha = CLAUDE_SHA256:sub(1, 8)
		local version_dir = string.format("%s/.local/share/claude/%s-%s", env.DST, CLAUDE_VERSION, short_sha)
		local claude_bin = version_dir .. "/claude"

		if unix.stat(claude_bin) then
			io.stderr:write("claude " .. CLAUDE_VERSION .. " already installed\n")
		else
			unix.makedirs(version_dir)

			local temp_file = "/tmp/claude-download-" .. os.time()
			util.spawn({"curl", "-fsSL", "-o", temp_file, CLAUDE_URL})

			local handle = io.popen(string.format("shasum -a 256 '%s'", temp_file), "r")
			local actual_sha256
			if handle then
				local output = handle:read("*l")
				handle:close()
				if output then
					actual_sha256 = output:match("^(%x+)")
				end
			end

			if actual_sha256 == CLAUDE_SHA256 then
				util.spawn({"mv", temp_file, claude_bin})
				unix.chmod(claude_bin, 0755)
			else
				io.stderr:write("error: claude binary checksum verification failed\n")
				unix.unlink(temp_file)
				return 1
			end
		end
	end

	local config = env.DST .. "/.claude.json"
	if unix.stat(config) then
		return 0
	end

	local claude_credentials = os.getenv("CLAUDE_CREDENTIALS")
	if claude_credentials then
		local creds_dir = env.DST .. "/.claude"
		unix.makedirs(creds_dir)
		local creds_file = creds_dir .. "/.credentials.json"
		local fd = unix.open(creds_file, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, 0600)
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

	local fd = unix.open(config, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, 0644)
	if fd and fd >= 0 then
		unix.write(fd, settings)
		unix.close(fd)
	end

	return 0
end

return {
	run = run,
	print_latest = print_latest,
}
