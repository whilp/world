local cosmo = require("cosmo")
local unix = cosmo.unix

local function run(env)
	if os.getenv("CODESPACES") == "true" then
		local CLAUDE_VERSION = "2.0.67"
		local CLAUDE_SHA256 = "b2a12279d5df3814f59000682a571edb771b73e89b4bd894101f01e3726726f3"
		local CLAUDE_URL = string.format(
			"https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases/%s/linux-x64/claude",
			CLAUDE_VERSION
		)
		local CLAUDE_BIN = env.DST .. "/.local/share/claude/bin/claude"

		local bin_dir = cosmo.path.dirname(CLAUDE_BIN)
		unix.makedirs(bin_dir)

		local temp_file = "/tmp/claude-download"
		local cmd = string.format("curl -fsSL -o %s %s", temp_file, CLAUDE_URL)
		os.execute(cmd)

		local verify_cmd = string.format("echo '%s  %s' | sha256sum -c -", CLAUDE_SHA256, temp_file)
		local ok, status, code = os.execute(verify_cmd)
		local success = (ok == true) or (ok == 0) or (code == 0)
		if success then
			os.execute(string.format("mv '%s' '%s'", temp_file, CLAUDE_BIN))
			unix.chmod(CLAUDE_BIN, 0755)
		else
			io.stderr:write("error: claude binary checksum verification failed\n")
			os.remove(temp_file)
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
}
