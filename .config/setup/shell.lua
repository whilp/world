local cosmo = require("cosmo")
local unix = cosmo.unix
local util = require("util")

local function run(env)
	local handle = io.popen("id -un", "r")
	if not handle then
		return 1
	end
	local username = handle:read("*l")
	handle:close()

	local zsh_path = unix.commandv("zsh")
	if zsh_path then
		util.spawn({"sudo", "chsh", username, "--shell", zsh_path})
	end

	local bashrc = path.join(env.DST, ".bashrc")
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
