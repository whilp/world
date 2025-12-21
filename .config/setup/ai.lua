local cosmo = require("cosmo")
local unix = cosmo.unix
local util = require("util")

local function run(env)
	unix.chdir(env.DST)

	if not unix.stat("./ai") then
		local remote_base = env.REMOTE:match("(.+)/[^/]+$")
		if not remote_base then
			return 0
		end

		local status = util.spawn({"git", "clone", path.join(remote_base, "ai"), "./ai"})
		if status ~= 0 then
			return 0
		end
	else
		unix.chdir("./ai")
		util.spawn({"git", "fetch"})
		unix.chdir(env.DST)
	end

	util.spawn({"claude", "plugin", "marketplace", "add", "./ai"})

	return 0
end

return {
	run = run,
}
