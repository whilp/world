-- type declarations for posix.signal

local type SignalHandler = function(signum: number)

local record signal
  -- signal numbers
  SIGINT: number
  SIGTERM: number
  SIGHUP: number
  SIGKILL: number
  SIGQUIT: number
  SIGUSR1: number
  SIGUSR2: number

  -- signal actions
  SIG_DFL: SignalHandler
  SIG_IGN: SignalHandler

  -- functions
  signal: function(sig: number, handler: SignalHandler): SignalHandler
  kill: function(pid: number, sig: number): boolean, string
  raise: function(sig: number): boolean
end

return signal
