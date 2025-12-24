local cosmo = require("cosmo")
local unix = cosmo.unix

local function spawn(argv, opts)
	opts = opts or {}
	local cmd = unix.commandv(argv[1])
	if not cmd then
		return nil, "command not found: " .. argv[1]
	end

	local pid = unix.fork()
	if pid == 0 then
		if opts.silent then
			local devnull = unix.open("/dev/null", unix.O_WRONLY)
			if devnull then
				unix.dup2(devnull, 1)
				unix.dup2(devnull, 2)
				unix.close(devnull)
			end
		end
		local full_argv = {cmd}
		for i = 2, #argv do
			table.insert(full_argv, argv[i])
		end
		unix.execve(cmd, full_argv, unix.environ())
		os.exit(127)
	else
		local wpid, status = unix.wait(pid)
		if unix.WIFEXITED(status) then
			return unix.WEXITSTATUS(status)
		end
		return nil, "process terminated abnormally"
	end
end

local function copy_tree(src, dst)
	local cp = unix.commandv("cp")
	if not cp then
		return nil, "cp command not found"
	end

	local pid = unix.fork()
	if pid == 0 then
		unix.execve(cp, {cp, "-ra", src, dst}, unix.environ())
		os.exit(127)
	else
		local wpid, status = unix.wait(pid)
		if unix.WIFEXITED(status) then
			return unix.WEXITSTATUS(status) == 0
		end
		return nil, "cp process terminated abnormally"
	end
end

return {
	spawn = spawn,
	copy_tree = copy_tree,
}
