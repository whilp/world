local cosmo = require("cosmo")
local unix = cosmo.unix

local function make_pipe(fd)
	if not fd then
		return nil
	end
	local pipe = {fd = fd}

	function pipe:write(data)
		return unix.write(self.fd, data)
	end

	function pipe:read(size)
		if size then
			return unix.read(self.fd, size)
		end
		local chunks = {}
		while true do
			local chunk = unix.read(self.fd, 65536)
			if not chunk or chunk == "" then
				break
			end
			table.insert(chunks, chunk)
		end
		return table.concat(chunks)
	end

	function pipe:close()
		if self.fd then
			unix.close(self.fd)
			self.fd = nil
		end
	end

	return pipe
end

local function spawn(argv, opts)
	opts = opts or {}
	local cmd
	if argv[1]:find("/") then
		cmd = argv[1]
	else
		cmd = unix.commandv(argv[1])
		if not cmd then
			return nil, "command not found: " .. argv[1]
		end
	end

	local stdin_r, stdin_w
	local stdin_is_fd = type(opts.stdin) == "number"
	if not stdin_is_fd then
		stdin_r, stdin_w = unix.pipe()
	end

	local stdout_r, stdout_w
	local stdout_is_fd = type(opts.stdout) == "number"
	if not stdout_is_fd then
		stdout_r, stdout_w = unix.pipe()
	end

	local stderr_r, stderr_w
	local stderr_is_fd = type(opts.stderr) == "number"
	if not stderr_is_fd then
		stderr_r, stderr_w = unix.pipe()
	end

	local pid = unix.fork()
	if pid == 0 then
		unix.close(0)
		if stdin_is_fd then
			unix.dup(opts.stdin)
		else
			unix.close(stdin_w)
			unix.dup(stdin_r)
			unix.close(stdin_r)
		end

		unix.close(1)
		if stdout_is_fd then
			unix.dup(opts.stdout)
		else
			unix.close(stdout_r)
			unix.dup(stdout_w)
			unix.close(stdout_w)
		end

		unix.close(2)
		if stderr_is_fd then
			unix.dup(opts.stderr)
		else
			unix.close(stderr_r)
			unix.dup(stderr_w)
			unix.close(stderr_w)
		end

		local full_argv = {cmd}
		for i = 2, #argv do
			table.insert(full_argv, argv[i])
		end
		local env = opts.env or unix.environ()
		unix.execve(cmd, full_argv, env)
		os.exit(127)
	end

	if not stdin_is_fd then
		unix.close(stdin_r)
	end
	if not stdout_is_fd then
		unix.close(stdout_w)
	end
	if not stderr_is_fd then
		unix.close(stderr_w)
	end

	local handle = {
		pid = pid,
		stdin = make_pipe(stdin_w),
		stdout = make_pipe(stdout_r),
		stderr = make_pipe(stderr_r),
	}

	function handle:wait()
		if self.stdin then self.stdin:close() end
		if self.stdout then self.stdout:read() end
		if self.stderr then self.stderr:read() end
		if self.stdout then self.stdout:close() end
		if self.stderr then self.stderr:close() end
		local _, status = unix.wait(self.pid)
		if unix.WIFEXITED(status) then
			return unix.WEXITSTATUS(status)
		end
		return nil, "process terminated abnormally"
	end

	function handle:read(size)
		if not self.stdout then
			return nil, "stdout not captured"
		end
		if self.stdin then self.stdin:close() end
		local out = self.stdout:read(size)
		if size then
			return out
		end
		local exit_code = self:wait()
		return exit_code == 0, out, exit_code
	end

	if type(opts.stdin) == "string" then
		handle.stdin:write(opts.stdin)
		handle.stdin:close()
	end

	return handle
end

return {
	spawn = spawn,
}
