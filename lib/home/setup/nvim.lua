local cosmo = require("cosmo")
local unix = cosmo.unix
local path = cosmo.path

local function run(env)
	unix.rmrf(path.join(env.DST, ".local", "state", "nvim", "swap"))
	return 0
end

return {
	run = run,
}
