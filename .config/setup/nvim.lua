local cosmo = require("cosmo")
local unix = cosmo.unix

local function rm_rf(path)
	local cmd = string.format("rm -rf %s", path)
	os.execute(cmd)
end

local function run(env)
	rm_rf(env.DST .. "/.local/state/nvim/swap")
	return 0
end

return {
	run = run,
}
