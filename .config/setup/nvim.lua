local cosmo = require("cosmo")
local unix = cosmo.unix

local function run(env)
	unix.rmrf(env.DST .. "/.local/state/nvim/swap")
	return 0
end

return {
	run = run,
}
