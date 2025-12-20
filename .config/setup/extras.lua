local cosmo = require("cosmo")
local unix = cosmo.unix

local function run(env)
	local remote_base = env.REMOTE:match("(.+)/[^/]+$")
	if not remote_base then
		return 0
	end

	local extras = remote_base .. "/extras"
	unix.chdir(env.DST)

	if not unix.stat("extras") then
		local cmd = string.format("git clone %s extras 2>/dev/null", extras)
		local status = os.execute(cmd)
		if status == 0 then
			unix.chdir("extras")
			if unix.stat("./setup.sh") and unix.access("./setup.sh", unix.X_OK) then
				os.execute("./setup.sh")
			end
		end
	else
		unix.chdir("./extras")
		os.execute("git fetch")
		if unix.stat("./setup.sh") and unix.access("./setup.sh", unix.X_OK) then
			os.execute("./setup.sh")
		end
	end

	return 0
end

return {
	run = run,
}
