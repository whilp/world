local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("spawn").spawn
local util = require("setup.util")

local function run(env)
	unix.rmrf(path.join(env.DST, ".git"))
	util.copy_tree(path.join(env.SRC, ".git"), path.join(env.DST, ".git"))
	unix.chdir(env.DST)
	spawn({"git", "checkout", "."}):wait()
	spawn({"git", "config", "user.email", "189851+whilp@users.noreply.github.com"}):wait()
	spawn({"git", "config", "core.fsmonitor", "false"}):wait()

	if unix.commandv("watchman") then
		local devnull = unix.open("/dev/null", unix.O_RDWR)
		spawn({"watchman", "watch-project", env.DST}, {stdin = devnull, stdout = devnull, stderr = devnull})
		unix.close(devnull)
	end

	return 0
end

return {
	run = run,
}
