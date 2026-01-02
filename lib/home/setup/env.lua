local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.spawn").spawn

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
		path.join(env.DST, "extras", "lua", "?.lua"),
		path.join(env.DST, "extras", "lua", "?", "init.lua"),
		path.join(env.DST, "extras", "lua", "3p", "?.lua"),
		path.join(env.DST, "lib", "?.lua"),
		path.join(env.DST, "lib", "?", "init.lua"),
		path.join(env.DST, "lib", "3p", "?.lua"),
		"",
	}
	env.LUA_PATH = table.concat(lua_path_parts, ";")

	env.SHELLINIT = path.join(env.DST, ".config", "shellinit")

	local ok, output = spawn({"git", "config", "--get", "remote.origin.url"}):read()
	if ok and output then
		env.REMOTE = output:gsub("%s+$", "")
	else
		env.REMOTE = ""
	end

	return env
end

return {
	get = get,
}
