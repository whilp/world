local cosmo = require("cosmo")
local unix = cosmo.unix
local util = require("util")

local function run(env)
	unix.rmrf(env.DST .. "/.git")
	util.copy_tree(env.SRC .. "/.git", env.DST .. "/.git")
	unix.chdir(env.DST)
	util.spawn({"git", "checkout", "."})
	util.spawn({"git", "config", "user.email", "189851+whilp@users.noreply.github.com"})
	util.spawn({"git", "config", "core.fsmonitor", "false"})

	if unix.commandv("watchman") then
		local pid = unix.fork()
		if pid == 0 then
			local watchman = unix.commandv("watchman")
			unix.execve(watchman, {watchman, "watch-project", env.DST}, unix.environ())
			os.exit(127)
		end
	end

	return 0
end

return {
	run = run,
}
