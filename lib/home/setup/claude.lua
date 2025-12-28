local cosmo = require("cosmo")
local unix = cosmo.unix
local path = cosmo.path
local spawn = require("spawn").spawn

local CLAUDE_BASE_URL = "https://storage.googleapis.com/" ..
	"claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819"

local function get_latest_version()
	local ok, output = spawn({"gh", "api", "repos/anthropics/claude-code/releases/latest", "--jq", ".tag_name"}):read()
	if not ok then
		io.stderr:write("error: failed to fetch latest version\n")
		return nil
	end
	local version = output:gsub("%s+$", "")
	if version then
		version = version:match("^v?(.+)$")
	end
	return version
end

local function get_sha256(url)
	local status, _, body = cosmo.Fetch(url, {maxresponse = 300 * 1024 * 1024})
	if not status then
		io.stderr:write("error: failed to download claude binary\n")
		return nil
	end
	if status ~= 200 then
		io.stderr:write("error: download failed with status " .. tostring(status) .. "\n")
		return nil
	end
	return cosmo.EncodeHex(cosmo.Sha256(body)):lower()
end

local function print_latest()
	local version = get_latest_version()
	if not version then
		return 1
	end

	local url = string.format("%s/claude-code-releases/%s/linux-x64/claude", CLAUDE_BASE_URL, version)

	io.stderr:write("fetching sha256 for linux-x64...\n")
	local sha256 = get_sha256(url)
	if not sha256 then
		return 1
	end

	local result = {
		version = version,
		url = CLAUDE_BASE_URL .. "/claude-code-releases/{version}/{platform}/claude",
		platforms = {
			["linux-x64"] = {sha = sha256},
		},
	}

	print("return " .. cosmo.EncodeLua(result, {pretty=true}))
	return 0
end

local function run(env)
	if os.getenv("CODESPACES") == "true" then
		local CLAUDE_VERSION = "2.0.74"
		local CLAUDE_SHA256 = "43065ff86a1b952225e42042bf4dfe9f6b72ff8ed91686a23add5396b1a11e80"
		local CLAUDE_URL = string.format(
			"%s/claude-code-releases/%s/linux-x64/claude", CLAUDE_BASE_URL, CLAUDE_VERSION)

		local short_sha = CLAUDE_SHA256:sub(1, 8)
		local version_dir = path.join(env.DST, ".local", "share", "claude", string.format("%s-%s", CLAUDE_VERSION, short_sha))
		local claude_bin = path.join(version_dir, "claude")

		if unix.stat(claude_bin) then
			io.stderr:write("claude " .. CLAUDE_VERSION .. " already installed\n")
		else
			unix.makedirs(version_dir)

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
	print_latest = print_latest,
}
