local unix = require("cosmo.unix")
local path = require("cosmo.path")

local function run(env)
	if os.getenv("CODESPACES") ~= "true" then
		return 0
	end

	local creation_log = "/workspaces/.codespaces/.persistedshare/creation.log"
	local setup_log = path.join(env.DST, "setup.log")

	if unix.stat(creation_log) then
		unix.unlink(setup_log)
		unix.symlink(creation_log, setup_log)
	end

	return 0
end

return {
	run = run,
}
