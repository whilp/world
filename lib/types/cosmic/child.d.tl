-- type declarations for cosmic.child

local record child
  record Pipe
    fd: number
    write: function(self: Pipe, data: string): number
    read: function(self: Pipe, size?: number): string
    close: function(self: Pipe)
  end

  record Handle
    pid: number
    stdin: Pipe
    stdout: Pipe
    stderr: Pipe
    wait: function(self: Handle): number, string
    read: function(self: Handle, size?: number): boolean | string, string, number
  end

  record Opts
    stdin: string | number
    stdout: number
    stderr: number
    env: {string}
  end

  spawn: function(argv: {string}, opts?: Opts): Handle, string
  metamethod __call: function(self: child, argv: {string}, opts?: Opts): Handle, string
end

return child
