-- cosmic.spawn: process spawning utilities
local unix = require("cosmo.unix")

local record Pipe
  fd: number
  write: function(self: Pipe, data: string): number
  read: function(self: Pipe, size?: number): string
  close: function(self: Pipe)
end

local record SpawnHandle
  pid: number
  stdin: Pipe
  stdout: Pipe
  stderr: Pipe
  wait: function(self: SpawnHandle): number, string
  read: function(self: SpawnHandle, size?: number): boolean | string, string, number
end

local record SpawnOpts
  stdin: string | number
  stdout: number
  stderr: number
  env: {string}
end

local function make_pipe(fd: number): Pipe
  if not fd then
    return nil
  end
  local pipe: Pipe = {fd = fd}

  function pipe:write(data: string): number
    return unix.write(self.fd, data)
  end

  function pipe:read(size?: number): string
    if size then
      return unix.read(self.fd, size)
    end
    local chunks: {string} = {}
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

local function spawn(argv: {string}, opts?: SpawnOpts): SpawnHandle, string
  opts = opts or {}

  if not (argv[1] as string):find("/") then
    local cmd = unix.commandv(argv[1])
    if not cmd then
      return nil, "command not found: " .. argv[1]
    end
  end

  local stdin_r: number
  local stdin_w: number
  local stdin_is_fd = type(opts.stdin) == "number"
  if not stdin_is_fd then
    stdin_r, stdin_w = unix.pipe()
  end

  local stdout_r: number
  local stdout_w: number
  local stdout_is_fd = type(opts.stdout) == "number"
  if not stdout_is_fd then
    stdout_r, stdout_w = unix.pipe()
  end

  local stderr_r: number
  local stderr_w: number
  local stderr_is_fd = type(opts.stderr) == "number"
  if not stderr_is_fd then
    stderr_r, stderr_w = unix.pipe()
  end

  local pid = unix.fork()
  if pid == 0 then
    -- stdin (fd 0)
    if stdin_is_fd then
      if opts.stdin ~= 0 then
        unix.close(0)
        unix.dup(opts.stdin as number)
      end
    else
      unix.close(0)
      unix.close(stdin_w)
      unix.dup(stdin_r)
      unix.close(stdin_r)
    end

    -- stdout (fd 1)
    if stdout_is_fd then
      if opts.stdout ~= 1 then
        unix.close(1)
        unix.dup(opts.stdout as number)
      end
    else
      unix.close(1)
      unix.close(stdout_r)
      unix.dup(stdout_w)
      unix.close(stdout_w)
    end

    -- stderr (fd 2)
    if stderr_is_fd then
      if opts.stderr ~= 2 then
        unix.close(2)
        unix.dup(opts.stderr as number)
      end
    else
      unix.close(2)
      unix.close(stderr_r)
      unix.dup(stderr_w)
      unix.close(stderr_w)
    end

    local env = opts.env or unix.environ()
    if (argv[1] as string):find("/") then
      unix.execve(argv[1], argv, env)
    else
      unix.execvpe(argv[1], argv, env)
    end
    os.exit(127)
  end

  if pid == nil then
    return nil, "fork failed"
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

  local handle: SpawnHandle = {
    pid = pid,
    stdin = make_pipe(stdin_w),
    stdout = make_pipe(stdout_r),
    stderr = make_pipe(stderr_r),
  }

  function handle:wait(): number, string
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

  function handle:read(size?: number): boolean | string, string, number
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
    handle.stdin:write(opts.stdin as string)
    handle.stdin:close()
  end

  return handle
end

local record SpawnModule
  spawn: function(argv: {string}, opts?: SpawnOpts): SpawnHandle, string
  metamethod __call: function(self: SpawnModule, argv: {string}, opts?: SpawnOpts): SpawnHandle, string
end

local M: SpawnModule = {
  spawn = spawn,
}

setmetatable(M, {
  __call = function(_: SpawnModule, argv: {string}, opts?: SpawnOpts): SpawnHandle, string
    return spawn(argv, opts)
  end,
})

return M
