local cosmo = require("cosmo")
local unix = cosmo.unix

local function run(env)
	local handle = io.popen("id -un", "r")
	if not handle then
		return 1
	end
	local username = handle:read("*l")
	handle:close()

	handle = io.popen("which zsh", "r")
	if not handle then
		return 1
	end
	local zsh_path = handle:read("*l")
	handle:close()

	if zsh_path and zsh_path ~= "" then
		os.execute(string.format("sudo chsh %s --shell %s", username, zsh_path))
	end

	local bashrc = env.DST .. "/.bashrc"
	local fd = unix.open(bashrc, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, 0644)
	if fd and fd >= 0 then
		unix.write(fd, "export SHELL=/bin/zsh\n")
		unix.close(fd)
	end

	return 0
end

return {
	run = run,
}
