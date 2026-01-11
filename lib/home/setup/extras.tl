local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn")

local env_module = require("setup.env")

local function run(env: env_module.Env): number
	local remote_base = env.REMOTE:match("(.+)/[^/]+$")
	if not remote_base then
		return 0
	end

	local extras = path.join(remote_base, "extras")
	unix.chdir(env.DST)

	if not unix.stat("extras") then
		local devnull = unix.open("/dev/null", unix.O_RDWR)
		local status = spawn({"git", "clone", extras, "extras"}, {stdout = devnull, stderr = devnull}):wait()
		unix.close(devnull)
		if status == 0 then
			unix.chdir("extras")
			if unix.stat("./setup.sh") and unix.access("./setup.sh", unix.X_OK) then
				spawn({"./setup.sh"}):wait()
			end
		end
	else
		unix.chdir("./extras")
		spawn({"git", "fetch"}):wait()
		if unix.stat("./setup.sh") and unix.access("./setup.sh", unix.X_OK) then
			spawn({"./setup.sh"}):wait()
		end
	end

	return 0
end

return {
	run = run,
}
