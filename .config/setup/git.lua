local cosmo = require("cosmo")
local unix = cosmo.unix

local function run(env)
	os.execute(string.format("rm -rf '%s'", env.DST .. "/.git"))
	os.execute(string.format("cp -ra '%s' '%s'", env.SRC .. "/.git", env.DST .. "/.git"))
	unix.chdir(env.DST)
	os.execute("git checkout .")
	os.execute("git config user.email 189851+whilp@users.noreply.github.com")
	os.execute("git config core.fsmonitor false")

	local handle = io.popen("command -v watchman >/dev/null 2>&1 && echo yes", "r")
	if handle then
		local has_watchman = handle:read("*l")
		handle:close()
		if has_watchman == "yes" then
			os.execute(string.format("watchman watch-project %s &", env.DST))
		end
	end

	return 0
end

return {
	run = run,
}
