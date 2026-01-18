-- type declarations for cosmic.spawn

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

local record spawn
  spawn: function(argv: {string}, opts?: SpawnOpts): SpawnHandle, string
  metamethod __call: function(self: spawn, argv: {string}, opts?: SpawnOpts): SpawnHandle, string
end

return spawn
