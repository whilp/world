local cosmo = require("cosmo")
local unix = cosmo.unix

local function get()
	local env = {}

	local cwd = unix.getcwd()
	if not cwd then
		io.stderr:write("error: failed to get current working directory\n")
		os.exit(1)
	end

	env.SRC = cwd
	env.DST = os.getenv("HOME")

	local path_parts = {
		env.DST .. "/.local/bootstrap/bin",
		env.DST .. "/.local/share/shimlink/bin",
		env.DST .. "/.local/bin",
		env.DST .. "/extras/bin",
		os.getenv("PATH") or "",
	}
	env.PATH = table.concat(path_parts, ":")

	local lua_path_parts = {
		env.DST .. "/.local/bootstrap/lib/lua/?.lua",
		env.DST .. "/.local/bootstrap/lib/lua/?/init.lua",
		env.DST .. "/.local/lib/lua/?.lua",
		env.DST .. "/.local/lib/lua/?/init.lua",
		"",
	}
	env.LUA_PATH = table.concat(lua_path_parts, ";")

	env.SHELLINIT = env.DST .. "/.config/shellinit"

	local handle = io.popen("git config --get remote.origin.url 2>/dev/null", "r")
	if handle then
		local remote = handle:read("*l")
		handle:close()
		env.REMOTE = remote or ""
	else
		env.REMOTE = ""
	end

	return env
end

return {
	get = get,
}
