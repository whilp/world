local cosmo = require("cosmo")
local unix = cosmo.unix

local function rm_rf(path)
	local cmd = string.format("rm -rf %s", path)
	os.execute(cmd)
end

local function cp_ra(src, dst)
	local cmd = string.format("cp -ra %s %s", src, dst)
	os.execute(cmd)
end

local function run(env)
	rm_rf(env.DST .. "/.git")
	cp_ra(env.SRC .. "/.git", env.DST .. "/.git")
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
