local cosmo = require("cosmo")
local unix = cosmo.unix

local function run(env)
	os.execute(string.format("rm -rf '%s'", env.DST .. "/.local/state/nvim/swap"))
	return 0
end

return {
	run = run,
}
