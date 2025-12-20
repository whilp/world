local cosmo = require("cosmo")
local unix = cosmo.unix

local env_module = require("env")
local backup = require("backup")
local git = require("git")
local shell = require("shell")
local codespace = require("codespace")
local claude = require("claude")
local nvim = require("nvim")
local extras = require("extras")
local ai = require("ai")

local function main()
	local env = env_module.get()

	backup.run(env)
	git.run(env)
	shell.run(env)
	codespace.run(env)

	local pids = {}

	local pid = unix.fork()
	if pid == 0 then
		claude.run(env)
		os.exit(0)
	else
		table.insert(pids, pid)
	end

	pid = unix.fork()
	if pid == 0 then
		nvim.run(env)
		os.exit(0)
	else
		table.insert(pids, pid)
	end

	pid = unix.fork()
	if pid == 0 then
		extras.run(env)
		os.exit(0)
	else
		table.insert(pids, pid)
	end

	pid = unix.fork()
	if pid == 0 then
		ai.run(env)
		os.exit(0)
	else
		table.insert(pids, pid)
	end

	for _, p in ipairs(pids) do
		unix.wait(p)
	end

	return 0
end

return {
	main = main,
}
