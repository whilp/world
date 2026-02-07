--check:false
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.child").spawn

local env_module = require("setup.env")

local function run(env: env_module.Env): number
	unix.chdir(env.DST)
	local e = unix.environ()

	if not unix.stat("./ai") then
		local remote_base = env.REMOTE:match("(.+)/[^/]+$")
		if not remote_base then
			return 0
		end

		local devnull = unix.open("/dev/null", unix.O_RDWR)
		local clone_url = path.join(remote_base, "ai")
		local status = spawn({"git", "clone", clone_url, "./ai"}, {stdout = devnull, stderr = devnull, env = e}):wait()
		unix.close(devnull)
		if status ~= 0 then
			return 0
		end
	else
		unix.chdir("./ai")
		spawn({"git", "fetch"}, {env = e}):wait()
		unix.chdir(env.DST)
	end

	spawn({"claude", "plugin", "marketplace", "add", "./ai"}, {env = e}):wait()

	return 0
end

return {
	run = run,
}
