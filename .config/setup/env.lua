local cosmo = require("cosmo")
local unix = cosmo.unix
local path = cosmo.path

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
		path.join(env.DST, ".local", "bootstrap", "bin"),
		path.join(env.DST, ".local", "bin"),
		path.join(env.DST, "extras", "bin"),
		os.getenv("PATH") or "",
	}
	env.PATH = table.concat(path_parts, ":")

	local lua_path_parts = {
		path.join(env.DST, ".local", "bootstrap", "lib", "lua", "?.lua"),
		path.join(env.DST, ".local", "bootstrap", "lib", "lua", "?", "init.lua"),
		path.join(env.DST, ".local", "lib", "lua", "?.lua"),
		path.join(env.DST, ".local", "lib", "lua", "?", "init.lua"),
		"",
	}
	env.LUA_PATH = table.concat(lua_path_parts, ";")

	env.SHELLINIT = path.join(env.DST, ".config", "shellinit")

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
