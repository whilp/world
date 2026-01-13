local unix = require("cosmo.unix")

local env_module = require("setup.env")
local backup = require("setup.backup")
local git = require("setup.git")
local shell = require("setup.shell")
local codespace = require("setup.codespace")
local claude = require("setup.claude")
local nvim = require("setup.nvim")
local extras = require("setup.extras")
local ai = require("setup.ai")

local function main(): number
	local env = env_module.get()

	backup.run(env)
	git.run(env)
	shell.run(env)
	codespace.run(env)

	local pids: {number} = {}

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
