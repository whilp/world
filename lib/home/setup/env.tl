--check:false
local unix = require("cosmo.unix")
local path = require("cosmo.path")
local spawn = require("cosmic.child").spawn

local record Env
	SRC: string
	DST: string
	PATH: string
	LUA_PATH: string
	SHELLINIT: string
	REMOTE: string
end

local function get(): Env
	local env: Env = {}

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

	local ok, output = spawn({"git", "config", "--get", "remote.origin.url"}, {env = unix.environ()}):read()
	if ok and output then
		env.REMOTE = (output as string):gsub("%s+$", "")
	else
		env.REMOTE = ""
	end

	return env
end

return {
	get = get,
	Env = Env,
}
