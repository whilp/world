local cosmo = require("cosmo")
local unix = cosmo.unix

local function run(env)
	unix.chdir(env.DST)

	if not unix.stat("./ai") then
		local remote_base = env.REMOTE:match("(.+)/[^/]+$")
		if not remote_base then
			return 0
		end

		local cmd = string.format("git clone %s/ai ./ai 2>/dev/null", remote_base)
		local status = os.execute(cmd)
		if status ~= 0 then
			return 0
		end
	else
		unix.chdir("./ai")
		os.execute("git fetch")
		unix.chdir(env.DST)
	end

	os.execute("claude plugin marketplace add ./ai 2>/dev/null || true")

	return 0
end

return {
	run = run,
}
