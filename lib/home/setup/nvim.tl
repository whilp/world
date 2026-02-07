--check:false
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local version = require("version")

local env_module = require("setup.env")

local function run(env: env_module.Env): number
	unix.rmrf(path.join(env.DST, ".local", "state", "nvim", "swap"))

	local share_dir = path.join(env.DST, ".local", "share")
	local nvim = version.find_latest("nvim", share_dir)
	if not nvim then
		return 0
	end

	local bundled_site = path.join(nvim.dir, "share", "nvim", "site")
	local st = unix.stat(bundled_site)
	if not st then
		return 0
	end

	local nvim_data = path.join(share_dir, "nvim")
	unix.makedirs(nvim_data, tonumber("755", 8))

	local site_link = path.join(nvim_data, "site")
	unix.rmrf(site_link)
	unix.symlink(bundled_site, site_link)

	return 0
end

return {
	run = run,
}
