local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")
local util = require("setup.util")

local env_module = require("setup.env")

local function run(env: env_module.Env): number
	unix.rmrf(path.join(env.DST, ".git"))
	util.copy_tree(path.join(env.SRC, ".git"), path.join(env.DST, ".git"))
	unix.chdir(env.DST)
	local e = unix.environ()
	spawn({"git", "checkout", "."}, {env = e}):wait()
	spawn({"git", "config", "user.email", "189851+whilp@users.noreply.github.com"}, {env = e}):wait()
	spawn({"git", "config", "core.fsmonitor", "false"}, {env = e}):wait()

	if unix.commandv("watchman") then
		local devnull = unix.open("/dev/null", unix.O_RDWR)
		spawn({"watchman", "watch-project", env.DST}, {stdin = devnull, stdout = devnull, stderr = devnull, env = e})
		unix.close(devnull)
	end

	return 0
end

return {
	run = run,
}
