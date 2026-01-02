local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local function run(env)
	local ok, output = spawn({"id", "-un"}):read()
	if not ok then
		return 1
	end
	local username = output:gsub("%s+$", "")

	local zsh_path = unix.commandv("zsh")
	if zsh_path then
		spawn({"sudo", "chsh", username, "--shell", zsh_path}):wait()
	end

	local bashrc = path.join(env.DST, ".bashrc")
	local fd = unix.open(bashrc, unix.O_WRONLY | unix.O_CREAT | unix.O_TRUNC, tonumber("0644", 8))
	if fd and fd >= 0 then
		unix.write(fd, "export SHELL=/bin/zsh\n")
		unix.close(fd)
	end

	return 0
end

return {
	run = run,
}
