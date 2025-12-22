# cosmo.unix

Unix system calls interface for Lua, provided by the `cosmo` module.

## Usage

```lua
local cosmo = require("cosmo")
local unix = cosmo.unix
```

## File operations

### open

```lua
fd, err = unix.open(path, flags [, mode])
```

Open a file and return file descriptor. `flags` must include one of `O_RDONLY`, `O_WRONLY`, or `O_RDWR`. Optional `mode` sets permissions for newly created files.

Common flags:
- `O_RDONLY` - read only
- `O_WRONLY` - write only
- `O_RDWR` - read and write
- `O_CREAT` - create if doesn't exist
- `O_TRUNC` - truncate to zero length
- `O_APPEND` - append mode
- `O_CLOEXEC` - close on exec
- `O_NONBLOCK` - non-blocking I/O

Returns file descriptor number on success, or `nil, err` on failure.

### read

```lua
data, err = unix.read(fd, count)
```

Read up to `count` bytes from file descriptor. Returns string of bytes read, or `nil, err` on failure. Returns empty string on EOF.

### write

```lua
n, err = unix.write(fd, data)
```

Write `data` string to file descriptor. Returns number of bytes written, or `nil, err` on failure.

### close

```lua
ok, err = unix.close(fd)
```

Close file descriptor. Returns `true` on success, or `nil, err` on failure.

### lseek

```lua
offset, err = unix.lseek(fd, offset, whence)
```

Reposition file offset. `whence` must be `SEEK_SET`, `SEEK_CUR`, or `SEEK_END`. Returns new offset from beginning of file, or `nil, err` on failure.

### pipe

```lua
read_fd, write_fd = unix.pipe()
```

Create a pipe. Returns read and write file descriptors.

### fcntl

```lua
result, err = unix.fcntl(fd, cmd [, arg])
```

File control operations. Common commands:
- `F_GETFD` - get file descriptor flags
- `F_SETFD` - set file descriptor flags (use `FD_CLOEXEC`)
- `F_GETFL` - get file status flags
- `F_SETFL` - set file status flags (use `O_NONBLOCK`)

### dup

```lua
newfd, err = unix.dup(fd)
```

Duplicate file descriptor. Returns new file descriptor, or `nil, err` on failure.

### sync

```lua
unix.sync()
```

Commit filesystem caches to disk.

### fsync

```lua
ok, err = unix.fsync(fd)
```

Synchronize file's in-core state with storage device. Returns `true` on success, or `nil, err` on failure.

### fdatasync

```lua
ok, err = unix.fdatasync(fd)
```

Like `fsync` but does not flush modified metadata. Returns `true` on success, or `nil, err` on failure.

## Directory operations

### getcwd

```lua
path, err = unix.getcwd()
```

Get current working directory. Returns absolute path string, or `nil, err` on failure.

### chdir

```lua
ok, err = unix.chdir(path)
```

Change current working directory. Returns `true` on success, or `nil, err` on failure.

### mkdir

```lua
ok, err = unix.mkdir(path [, mode])
```

Create directory. Optional `mode` defaults to `0777`. Returns `true` on success, or `nil, err` on failure.

### makedirs

```lua
ok, err = unix.makedirs(path [, mode])
```

Create directory and any missing parent directories (like `mkdir -p`). Returns `true` on success, or `nil, err` on failure.

### rmdir

```lua
ok, err = unix.rmdir(path)
```

Remove empty directory. Returns `true` on success, or `nil, err` on failure.

### rmrf

```lua
ok, err = unix.rmrf(path)
```

Recursively remove directory and its contents (like `rm -rf`). Returns `true` on success, or `nil, err` on failure.

### opendir

```lua
iter = unix.opendir(path)
```

Open directory and return iterator. Use in `for` loop:

```lua
for name, kind in unix.opendir(".") do
  print(name, kind)
end
```

`kind` is one of:
- `DT_REG` (8) - regular file
- `DT_DIR` (4) - directory
- `DT_LNK` (10) - symbolic link
- `DT_FIFO` (1) - FIFO
- `DT_CHR` (2) - character device
- `DT_BLK` (6) - block device
- `DT_SOCK` (12) - socket
- `DT_UNKNOWN` (0) - unknown

### fdopendir

```lua
iter = unix.fdopendir(fd)
```

Like `opendir` but takes file descriptor instead of path.

## File metadata

### stat

```lua
st, err = unix.stat(path)
```

Get file status. Returns stat object with methods:
- `st:mode()` - file type and permissions
- `st:size()` - file size in bytes
- `st:ino()` - inode number
- `st:dev()` - device ID
- `st:nlink()` - number of hard links
- `st:uid()` - user ID of owner
- `st:gid()` - group ID of owner
- `st:rdev()` - device ID (if special file)
- `st:blksize()` - block size for filesystem I/O
- `st:blocks()` - number of 512B blocks allocated
- `st:atim()` - access time (seconds since epoch)
- `st:mtim()` - modification time (seconds since epoch)
- `st:ctim()` - status change time (seconds since epoch)

Returns `nil, err` on failure.

Test file type with mode:
```lua
local st = unix.stat(path)
if st and unix.S_ISDIR(st:mode()) then
  print("is directory")
end
```

### fstat

```lua
st, err = unix.fstat(fd)
```

Like `stat` but takes file descriptor instead of path.

### lstat

```lua
st, err = unix.lstat(path)
```

Like `stat` but does not follow symbolic links.

### S_ISREG

```lua
bool = unix.S_ISREG(mode)
```

Test if mode indicates regular file.

### S_ISDIR

```lua
bool = unix.S_ISDIR(mode)
```

Test if mode indicates directory.

### S_ISLNK

```lua
bool = unix.S_ISLNK(mode)
```

Test if mode indicates symbolic link.

### S_ISFIFO

```lua
bool = unix.S_ISFIFO(mode)
```

Test if mode indicates FIFO.

### S_ISCHR

```lua
bool = unix.S_ISCHR(mode)
```

Test if mode indicates character device.

### S_ISBLK

```lua
bool = unix.S_ISBLK(mode)
```

Test if mode indicates block device.

### S_ISSOCK

```lua
bool = unix.S_ISSOCK(mode)
```

Test if mode indicates socket.

### access

```lua
ok, err = unix.access(path, mode)
```

Check file accessibility. `mode` is bitwise OR of:
- `F_OK` - file exists
- `R_OK` - readable
- `W_OK` - writable
- `X_OK` - executable

Returns `true` if accessible, or `nil, err` on failure.

### chmod

```lua
ok, err = unix.chmod(path, mode)
```

Change file permissions. Returns `true` on success, or `nil, err` on failure.

### chown

```lua
ok, err = unix.chown(path, uid, gid)
```

Change file ownership. Returns `true` on success, or `nil, err` on failure.

### link

```lua
ok, err = unix.link(oldpath, newpath)
```

Create hard link. Returns `true` on success, or `nil, err` on failure.

### symlink

```lua
ok, err = unix.symlink(target, linkpath)
```

Create symbolic link. Returns `true` on success, or `nil, err` on failure.

### readlink

```lua
target, err = unix.readlink(path)
```

Read symbolic link target. Returns target path string, or `nil, err` on failure.

### realpath

```lua
path, err = unix.realpath(path)
```

Resolve symbolic links and return absolute path. Returns canonical path, or `nil, err` on failure.

### rename

```lua
ok, err = unix.rename(oldpath, newpath)
```

Rename file or directory. Returns `true` on success, or `nil, err` on failure.

### unlink

```lua
ok, err = unix.unlink(path)
```

Delete file. Returns `true` on success, or `nil, err` on failure.

### truncate

```lua
ok, err = unix.truncate(path, length)
```

Truncate file to specified length. Returns `true` on success, or `nil, err` on failure.

### ftruncate

```lua
ok, err = unix.ftruncate(fd, length)
```

Like `truncate` but takes file descriptor instead of path.

### umask

```lua
oldmask = unix.umask(mask)
```

Set file mode creation mask. Returns previous mask.

### utimensat

```lua
ok, err = unix.utimensat(dirfd, path, atime, mtime, flags)
```

Change file timestamps. Use `AT_FDCWD` for `dirfd` to use current directory. Times are in seconds since epoch. Use `UTIME_NOW` or `UTIME_OMIT` for special handling. Returns `true` on success, or `nil, err` on failure.

### futimens

```lua
ok, err = unix.futimens(fd, atime, mtime)
```

Like `utimensat` but takes file descriptor.

### statfs

```lua
st, err = unix.statfs(path)
```

Get filesystem statistics. Returns statfs object, or `nil, err` on failure.

### fstatfs

```lua
st, err = unix.fstatfs(fd)
```

Like `statfs` but takes file descriptor instead of path.

## Process operations

### fork

```lua
pid = unix.fork()
```

Create child process. Returns:
- `0` in child process
- child PID in parent process
- `nil, err` on failure

### execve

```lua
unix.execve(path, args, env)
```

Replace current process. `args` is array of argument strings (including `args[1]` as program name). `env` is array of `"KEY=value"` strings. Does not return on success (process is replaced).

### exit

```lua
unix.exit(status)
```

Terminate process immediately with exit status. Does not return.

### wait

```lua
pid, status = unix.wait()
```

Wait for any child process to terminate. Returns child PID and exit status.

### WIFEXITED

```lua
bool = unix.WIFEXITED(status)
```

Test if child exited normally.

### WEXITSTATUS

```lua
code = unix.WEXITSTATUS(status)
```

Extract exit code from status (only valid if `WIFEXITED` is true).

### WIFSIGNALED

```lua
bool = unix.WIFSIGNALED(status)
```

Test if child terminated by signal.

### WTERMSIG

```lua
signum = unix.WTERMSIG(status)
```

Extract signal number from status (only valid if `WIFSIGNALED` is true).

### getpid

```lua
pid = unix.getpid()
```

Get process ID.

### getppid

```lua
pid = unix.getppid()
```

Get parent process ID.

### getuid

```lua
uid = unix.getuid()
```

Get real user ID.

### geteuid

```lua
uid = unix.geteuid()
```

Get effective user ID.

### getgid

```lua
gid = unix.getgid()
```

Get real group ID.

### getegid

```lua
gid = unix.getegid()
```

Get effective group ID.

### setuid

```lua
ok, err = unix.setuid(uid)
```

Set user ID. Returns `true` on success, or `nil, err` on failure.

### setgid

```lua
ok, err = unix.setgid(gid)
```

Set group ID. Returns `true` on success, or `nil, err` on failure.

### setresuid

```lua
ok, err = unix.setresuid(ruid, euid, suid)
```

Set real, effective, and saved user IDs. Returns `true` on success, or `nil, err` on failure.

### setresgid

```lua
ok, err = unix.setresgid(rgid, egid, sgid)
```

Set real, effective, and saved group IDs. Returns `true` on success, or `nil, err` on failure.

### setfsuid

```lua
olduid = unix.setfsuid(uid)
```

Set filesystem user ID. Returns previous UID.

### setfsgid

```lua
oldgid = unix.setfsgid(gid)
```

Set filesystem group ID. Returns previous GID.

### getpgid

```lua
pgid, err = unix.getpgid(pid)
```

Get process group ID of process. Returns PGID, or `nil, err` on failure.

### getpgrp

```lua
pgid = unix.getpgrp()
```

Get process group ID of current process.

### setpgid

```lua
ok, err = unix.setpgid(pid, pgid)
```

Set process group ID. Returns `true` on success, or `nil, err` on failure.

### setpgrp

```lua
ok, err = unix.setpgrp()
```

Set process group ID to PID. Returns `true` on success, or `nil, err` on failure.

### getsid

```lua
sid, err = unix.getsid(pid)
```

Get session ID of process. Returns SID, or `nil, err` on failure.

### setsid

```lua
sid, err = unix.setsid()
```

Create new session. Returns session ID, or `nil, err` on failure.

### environ

```lua
env = unix.environ()
```

Get environment variables as array of `"KEY=value"` strings.

Example:
```lua
local env = unix.environ()
-- Replace existing variable or append if not found
local found = false
for i, v in ipairs(env) do
  if v:match("^CUSTOM_VAR=") then
    env[i] = "CUSTOM_VAR=custom_value"
    found = true
    break
  end
end
if not found then
  table.insert(env, "CUSTOM_VAR=custom_value")
end
unix.execve("/usr/bin/env", {"env"}, env)
```

### chroot

```lua
ok, err = unix.chroot(path)
```

Change root directory. Returns `true` on success, or `nil, err` on failure.

## Signals

### kill

```lua
ok, err = unix.kill(pid, signum)
```

Send signal to process. Returns `true` on success, or `nil, err` on failure.

### raise

```lua
ok, err = unix.raise(signum)
```

Send signal to current process. Returns `true` on success, or `nil, err` on failure.

### sigaction

```lua
ok, err = unix.sigaction(signum, handler)
```

Set signal handler. `handler` is a Lua function or `SIG_DFL`/`SIG_IGN`. Returns `true` on success, or `nil, err` on failure.

Example:
```lua
unix.sigaction(unix.SIGINT, function(signum)
  print("Caught SIGINT")
  os.exit(128 + signum)
end)
```

### Sigset

```lua
set = unix.Sigset()
```

Create signal set object for use with `sigprocmask`.

### sigprocmask

```lua
oldset, err = unix.sigprocmask(how, set)
```

Change blocked signal mask. `how` is one of:
- `SIG_BLOCK` - add signals to mask
- `SIG_UNBLOCK` - remove signals from mask
- `SIG_SETMASK` - set mask

Returns previous signal set, or `nil, err` on failure.

### sigpending

```lua
set, err = unix.sigpending()
```

Get set of pending signals. Returns signal set, or `nil, err` on failure.

### sigsuspend

```lua
unix.sigsuspend(set)
```

Temporarily replace signal mask and suspend process until signal is caught.

### strsignal

```lua
name = unix.strsignal(signum)
```

Get signal name string.

Signal constants:
- `SIGHUP`, `SIGINT`, `SIGQUIT`, `SIGILL`, `SIGTRAP`, `SIGABRT`, `SIGBUS`, `SIGFPE`
- `SIGKILL`, `SIGUSR1`, `SIGSEGV`, `SIGUSR2`, `SIGPIPE`, `SIGALRM`, `SIGTERM`
- `SIGCHLD`, `SIGCONT`, `SIGSTOP`, `SIGTSTP`, `SIGTTIN`, `SIGTTOU`, `SIGURG`
- `SIGXCPU`, `SIGXFSZ`, `SIGVTALRM`, `SIGPROF`, `SIGWINCH`, `SIGIO`, `SIGPWR`
- `SIGSYS`, `SIGEMT`, `SIGINFO`, `SIGTHR`, `SIGSTKFLT`

## Time

### clock_gettime

```lua
seconds, err = unix.clock_gettime(clock_id)
```

Get time from specified clock. Returns seconds since epoch (may include fractional seconds), or `nil, err` on failure.

Clock IDs:
- `CLOCK_REALTIME` - system-wide real-time clock
- `CLOCK_MONOTONIC` - monotonic clock (not affected by time adjustments)
- `CLOCK_PROCESS_CPUTIME_ID` - per-process CPU time
- `CLOCK_THREAD_CPUTIME_ID` - per-thread CPU time
- `CLOCK_BOOTTIME` - time since boot (including suspend)
- `CLOCK_REALTIME_COARSE` - faster but less precise realtime
- `CLOCK_MONOTONIC_COARSE` - faster but less precise monotonic
- `CLOCK_MONOTONIC_RAW` - raw monotonic time

### nanosleep

```lua
ok, err = unix.nanosleep(seconds)
```

Sleep for specified duration (supports fractional seconds). Returns `true` on success, or `nil, err` on failure.

### gmtime

```lua
t = unix.gmtime(seconds)
```

Convert seconds since epoch to UTC time table.

### localtime

```lua
t = unix.localtime(seconds)
```

Convert seconds since epoch to local time table.

### setitimer

```lua
ok, err = unix.setitimer(which, interval, value)
```

Set interval timer. `which` is one of:
- `ITIMER_REAL` - real time (sends `SIGALRM`)
- `ITIMER_VIRTUAL` - process CPU time (sends `SIGVTALRM`)
- `ITIMER_PROF` - process and system CPU time (sends `SIGPROF`)

Returns `true` on success, or `nil, err` on failure.

## Network

### socket

```lua
fd, err = unix.socket(domain, type, protocol)
```

Create socket. Common values:
- `domain`: `AF_UNIX`, `AF_INET`, `AF_INET6`
- `type`: `SOCK_STREAM`, `SOCK_DGRAM`, `SOCK_RAW` (can OR with `SOCK_NONBLOCK`, `SOCK_CLOEXEC`)
- `protocol`: `0` (auto), `IPPROTO_TCP`, `IPPROTO_UDP`, `IPPROTO_ICMP`

Returns socket file descriptor, or `nil, err` on failure.

### socketpair

```lua
fd1, fd2 = unix.socketpair(domain, type, protocol)
```

Create connected socket pair (like `pipe` but bidirectional).

### bind

```lua
ok, err = unix.bind(fd, address)
```

Bind socket to address. Returns `true` on success, or `nil, err` on failure.

### listen

```lua
ok, err = unix.listen(fd, backlog)
```

Listen for connections. `backlog` is maximum queue length. Returns `true` on success, or `nil, err` on failure.

### accept

```lua
clientfd, addr, err = unix.accept(fd)
```

Accept connection. Returns client socket and address, or `nil, nil, err` on failure.

### connect

```lua
ok, err = unix.connect(fd, address)
```

Connect socket to address. Returns `true` on success, or `nil, err` on failure.

### send

```lua
n, err = unix.send(fd, data, flags)
```

Send data on socket. Returns bytes sent, or `nil, err` on failure.

### recv

```lua
data, err = unix.recv(fd, count, flags)
```

Receive data from socket. Returns data string, or `nil, err` on failure.

### sendto

```lua
n, err = unix.sendto(fd, data, flags, address)
```

Send datagram to address. Returns bytes sent, or `nil, err` on failure.

### recvfrom

```lua
data, addr, err = unix.recvfrom(fd, count, flags)
```

Receive datagram. Returns data and sender address, or `nil, nil, err` on failure.

### shutdown

```lua
ok, err = unix.shutdown(fd, how)
```

Shut down part of socket. `how` is one of:
- `SHUT_RD` - further receives disallowed
- `SHUT_WR` - further sends disallowed
- `SHUT_RDWR` - both sends and receives disallowed

Returns `true` on success, or `nil, err` on failure.

### getsockname

```lua
addr, err = unix.getsockname(fd)
```

Get socket address. Returns address, or `nil, err` on failure.

### getpeername

```lua
addr, err = unix.getpeername(fd)
```

Get peer address. Returns address, or `nil, err` on failure.

### setsockopt

```lua
ok, err = unix.setsockopt(fd, level, optname, value)
```

Set socket option. Returns `true` on success, or `nil, err` on failure.

Common levels: `SOL_SOCKET`, `SOL_IP`, `SOL_TCP`, `SOL_UDP`

Socket options (`SOL_SOCKET`):
- `SO_REUSEADDR`, `SO_REUSEPORT`, `SO_KEEPALIVE`, `SO_BROADCAST`
- `SO_RCVBUF`, `SO_SNDBUF`, `SO_RCVTIMEO`, `SO_SNDTIMEO`
- `SO_LINGER`, `SO_OOBINLINE`, `SO_DEBUG`, `SO_DONTROUTE`

TCP options (`SOL_TCP`):
- `TCP_NODELAY`, `TCP_CORK`, `TCP_KEEPIDLE`, `TCP_KEEPINTVL`, `TCP_KEEPCNT`
- `TCP_FASTOPEN`, `TCP_QUICKACK`, `TCP_DEFER_ACCEPT`

IP options (`SOL_IP`):
- `IP_TTL`, `IP_TOS`, `IP_MULTICAST_TTL`, `IP_MULTICAST_LOOP`
- `IP_ADD_MEMBERSHIP`, `IP_DROP_MEMBERSHIP`

### getsockopt

```lua
value, err = unix.getsockopt(fd, level, optname)
```

Get socket option. Returns value, or `nil, err` on failure.

### poll

```lua
count, err = unix.poll(fds, timeout)
```

Wait for events on file descriptors. `fds` is array of tables with:
- `fd` - file descriptor
- `events` - requested events (bitwise OR of `POLLIN`, `POLLOUT`, `POLLPRI`)
- `revents` - returned events (set by poll)

`timeout` is in milliseconds (-1 for infinite). Returns number of ready descriptors, or `nil, err` on failure.

Poll events:
- `POLLIN` - data available to read
- `POLLOUT` - ready for writing
- `POLLPRI` - urgent data available
- `POLLERR` - error condition
- `POLLHUP` - hang up
- `POLLNVAL` - invalid request

## Resource limits

### getrlimit

```lua
soft, hard, err = unix.getrlimit(resource)
```

Get resource limits. Returns soft and hard limits, or `nil, nil, err` on failure.

Resources:
- `RLIMIT_CPU` - CPU time in seconds
- `RLIMIT_FSIZE` - maximum file size
- `RLIMIT_NOFILE` - maximum number of open files
- `RLIMIT_NPROC` - maximum number of processes
- `RLIMIT_AS` - maximum address space
- `RLIMIT_RSS` - maximum resident set size

### setrlimit

```lua
ok, err = unix.setrlimit(resource, soft, hard)
```

Set resource limits. Returns `true` on success, or `nil, err` on failure.

### getrusage

```lua
usage, err = unix.getrusage(who)
```

Get resource usage. `who` is one of:
- `RUSAGE_SELF` - current process
- `RUSAGE_CHILDREN` - terminated and waited-for children
- `RUSAGE_THREAD` - current thread
- `RUSAGE_BOTH` - self and children

Returns usage object, or `nil, err` on failure.

## Terminal

### isatty

```lua
bool = unix.isatty(fd)
```

Test if file descriptor refers to terminal.

### tiocgwinsz

```lua
rows, cols, err = unix.tiocgwinsz(fd)
```

Get terminal window size. Returns rows and columns, or `nil, nil, err` on failure.

### gethostname

```lua
name, err = unix.gethostname()
```

Get hostname. Returns hostname string, or `nil, err` on failure.

## Security

### pledge

```lua
ok, err = unix.pledge(promises, execpromises)
```

Restrict system operations (OpenBSD pledge / Cosmopolitan unveil-like). Returns `true` on success, or `nil, err` on failure.

### unveil

```lua
ok, err = unix.unveil(path, permissions)
```

Unveil filesystem path with permissions. `permissions` is string like `"r"` (read), `"w"` (write), `"x"` (execute), `"c"` (create). Returns `true` on success, or `nil, err` on failure.

## Memory

### mapshared

```lua
addr, err = unix.mapshared(size)
```

Create shared memory mapping. Returns memory address, or `nil, err` on failure.

## Miscellaneous

### commandv

```lua
path, err = unix.commandv(name)
```

Find command in PATH (like `which`). Returns absolute path, or `nil, err` if not found.

### tmpfd

```lua
fd, err = unix.tmpfd()
```

Create temporary file descriptor. Returns fd, or `nil, err` on failure.

### verynice

```lua
ok = unix.verynice()
```

Lower process priority to minimum.

### sched_yield

```lua
ok, err = unix.sched_yield()
```

Yield CPU to other processes. Returns `true` on success, or `nil, err` on failure.

### syslog

```lua
unix.syslog(priority, message)
```

Write message to syslog.

Priority levels:
- `LOG_EMERG`, `LOG_ALERT`, `LOG_CRIT`, `LOG_ERR`
- `LOG_WARNING`, `LOG_NOTICE`, `LOG_INFO`, `LOG_DEBUG`

### siocgifconf

```lua
interfaces, err = unix.siocgifconf()
```

Get network interface configuration. Returns array of interface names, or `nil, err` on failure.

### major

```lua
major = unix.major(dev)
```

Extract major device number.

### minor

```lua
minor = unix.minor(dev)
```

Extract minor device number.

## Error codes

All error codes are available as constants:
- `EACCES`, `EADDRINUSE`, `EADDRNOTAVAIL`, `EAFNOSUPPORT`, `EAGAIN`, `EALREADY`
- `EBADF`, `EBADMSG`, `EBUSY`, `ECANCELED`, `ECHILD`, `ECONNABORTED`, `ECONNREFUSED`, `ECONNRESET`
- `EDEADLK`, `EDESTADDRREQ`, `EDOM`, `EDQUOT`, `EEXIST`, `EFAULT`, `EFBIG`
- `EHOSTDOWN`, `EHOSTUNREACH`, `EIDRM`, `EILSEQ`, `EINPROGRESS`, `EINTR`, `EINVAL`, `EIO`
- `EISCONN`, `EISDIR`, `ELOOP`, `EMFILE`, `EMLINK`, `EMSGSIZE`, `ENAMETOOLONG`
- `ENETDOWN`, `ENETRESET`, `ENETUNREACH`, `ENFILE`, `ENOBUFS`, `ENODEV`, `ENOENT`, `ENOEXEC`
- `ENOLCK`, `ENOMEM`, `ENOMSG`, `ENOPROTOOPT`, `ENOSPC`, `ENOSYS`, `ENOTBLK`, `ENOTCONN`
- `ENOTDIR`, `ENOTEMPTY`, `ENOTRECOVERABLE`, `ENOTSOCK`, `ENOTSUP`, `ENOTTY`, `ENXIO`
- `EOPNOTSUPP`, `EOVERFLOW`, `EOWNERDEAD`, `EPERM`, `EPFNOSUPPORT`, `EPIPE`, `EPROTO`
- `EPROTONOSUPPORT`, `EPROTOTYPE`, `ERANGE`, `EREMOTE`, `EROFS`, `ESHUTDOWN`, `ESPIPE`
- `ESRCH`, `ESTALE`, `ETIMEDOUT`, `ETXTBSY`, `EXDEV`

## Constants

Common constants are documented in context above. All standard POSIX constants are available including file flags, signals, socket options, and error codes.
