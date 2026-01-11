-- type declarations for daemonize module

local record daemonize
  write_pidfile: function(path: string): boolean, string
  acquire_lock: function(path: string): number, string
  redirect_output: function(stdout_path: string, stderr_path: string, append: boolean): boolean, string
end

return daemonize
