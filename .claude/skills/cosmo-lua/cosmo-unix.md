# cosmo.unix

### Dir

```lua
unix.Dir
```

The starting byte index from which memory is copied, which defaults to zero.
If `bytes` is none or nil, then the nul-terminated string at
`offset` is returned. You may specify `bytes` to safely read
binary data.
This operation happens atomically. Each shared mapping has a
single lock which is used to synchronize reads and writes to
that specific map. To make it scale, create additional maps.
Writes bytes to memory region.
`offset` is the starting byte index to which memory is copied,
which defaults to zero.
If `bytes` is none or nil, then an implicit nil-terminator
will be included after your `data` so things like json can
be easily serialized to shared memory.
This operation happens atomically. Each shared mapping has a
single lock which is used to synchronize reads and writes to
that specific map. To make it scale, create additional maps.
Loads word from memory region.
This operation is atomic and has relaxed barrier semantics.
Stores word from memory region.
This operation is atomic and has relaxed barrier semantics.
Exchanges value.
This sets word at `word_index` to `value` and returns the value
previously held in by the word.
This operation is atomic and provides the same memory barrier
semantics as the aligned x86 LOCK XCHG instruction.
Compares and exchanges value.
This inspects the word at `word_index` and if its value is the same
as `old` then it'll be replaced by the value `new`, in which case
`true, old` shall be returned. If a different value was held at
word, then `false` shall be returned along with the word.
This operation happens atomically and provides the same memory
barrier semantics as the aligned x86 LOCK CMPXCHG instruction.
Fetches then adds value.
This method modifies the word at `word_index` to contain the sum of
value and the `value` paremeter. This method then returns the value
as it existed before the addition was performed.
This operation is atomic and provides the same memory barrier
semantics as the aligned x86 LOCK XADD instruction.
Fetches and bitwise ands value.
This operation happens atomically and provides the same memory
barrier ordering semantics as its x86 implementation.
Fetches and bitwise ors value.
This operation happens atomically and provides the same memory
barrier ordering semantics as its x86 implementation.
Fetches and bitwise xors value.
This operation happens atomically and provides the same memory
barrier ordering semantics as its x86 implementation.
Waits for word to have a different value.
This method asks the kernel to suspend the process until either the
absolute deadline expires or we're woken up by another process that
calls `unix.Memory:wake()`.
The `expect` parameter is used only upon entry to synchronize the
transition to kernelspace. The kernel doesn't actually poll the
memory location. It uses `expect` to make sure the process doesn't
get added to the wait list unless it's sure that it needs to wait,
since the kernel can only control the ordering of wait / wake calls
across processes.
The default behavior is to wait until the heat death of the universe
if necessary. You may alternatively specify an absolute deadline. If
it's less than or equal to the value returned by clock_gettime, then
this routine is non-blocking. Otherwise we'll block at most until
the current time reaches the absolute deadline.
Futexes are currently supported on Linux, FreeBSD, OpenBSD. On other
platforms this method calls sched_yield() and will either (1) return
unix.EINTR if a deadline is specified, otherwise (2) 0 is returned.
This means futexes will *work* on Windows, Mac, and NetBSD but they
won't be scalable in terms of CPU usage when many processes wait on
one process that holds a lock for a long time. In the future we may
polyfill futexes in userspace for these platforms to improve things
for folks who've adopted this api. If lock scalability is something
you need on Windows and MacOS today, then consider fcntl() which is
well-supported on all supported platforms but requires using files.
Please test your use case though, because it's kind of an edge case
to have the scenario above, and chances are this op will work fine.
`EINTR` if a signal is delivered while waiting on deadline. Callers
should use futexes inside a loop that is able to cope with spurious
wakeups. We don't actually guarantee the value at word has in fact
changed when this returns.
`EAGAIN` is raised if, upon entry, the word at `word_index` had a
different value than what's specified at `expect`.
`ETIMEDOUT` is raised when the absolute deadline expires.
Wakes other processes waiting on word.
This method may be used to signal or broadcast to waiters. The
`count` specifies the number of processes that should be woken,
which defaults to `INT_MAX`.
The return value is the number of processes that were actually woken
as a result of the system call. No failure conditions are defined.
`unix.Dir` objects are created by `opendir()` or `fdopendir()`.

### Errno

```lua
unix.Errno
```

Adds signal to bitset.
Removes signal from bitset.
Sets all bits in signal bitset to `true`.
Sets all bits in signal bitset to `false`.
This object is returned by system calls that fail. We prefer returning
an object because for many system calls, an error is part their normal
operation. For example, it's often desirable to use the `errno()` method
when performing a `read()` to check for EINTR.

### Memory

```lua
unix.Memory
```

unix.Memory encapsulates memory that's shared across fork() and
this module provides the fundamental synchronization primitives
Redbean memory maps may be used in two ways:
1. as an array of bytes a.k.a. a string
2. as an array of words a.k.a. integers
They're aliased, union, or overlapped views of the same memory.
For example if you write a string to your memory region, you'll
be able to read it back as an integer.
Reads, writes, and word operations will throw an exception if a
memory boundary error or overflow occurs.

### Rusage

```lua
unix.Rusage
```

Closes directory stream object and associated its file descriptor.
This is called automatically by the garbage collector.
This may be called multiple times.
Reads entry from directory stream.
Returns `nil` if there are no more entries.
On error, `nil` will be returned and `errno` will be non-nil.
`kind` can be any of:
- `DT_REG`: file is a regular file
- `DT_DIR`: file is a directory
- `DT_BLK`: file is a block device
- `DT_LNK`: file is a symbolic link
- `DT_CHR`: file is a character device
- `DT_FIFO`: file is a named pipe
- `DT_SOCK`: file is a named socket
- `DT_UNKNOWN`
Note: This function also serves as the `__call` metamethod, so that
`unix.Dir` objects may be used as a for loop iterator.
Returns `EOPNOTSUPP` if using a `/zip/...` path or if using Windows NT.
Resets stream back to beginning.
`unix.Rusage` objects are created by `wait()` or `getrusage()`.

### Sigset

```lua
unix.Sigset(sig, ...)
```

Contains file type and permissions.
For example, `0010644` is what you might see for a file and
`0040755` is what you might see for a directory.
To determine the file type:
- `unix.S_ISREG(st:mode())` means regular file
- `unix.S_ISDIR(st:mode())` means directory
- `unix.S_ISLNK(st:mode())` means symbolic link
- `unix.S_ISCHR(st:mode())` means character device
- `unix.S_ISBLK(st:mode())` means block device
- `unix.S_ISFIFO(st:mode())` means fifo or pipe
- `unix.S_ISSOCK(st:mode())` means socket
File birth time.
This field should be accurate on Apple, Windows, and BSDs. On Linux
this is the minimum of atim/mtim/ctim. On Windows NT nanos is only
accurate to hectonanoseconds.
Here's an example of how you might print a file timestamp:
st = assert(unix.stat('/etc/passwd'))
unixts, nanos = st:birthtim()
year,mon,mday,hour,min,sec,gmtoffsec = unix.localtime(unixts)
Write('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%.9d%+.2d%.2d % {
year, mon, mday, hour, min, sec, nanos,
gmtoffsec / (60 * 60), math.abs(gmtoffsec) % 60})
Please note that file systems are sometimes mounted with `noatime`
out of concern for i/o performance. Linux also provides `O_NOATIME`
as an option for open().
On Windows NT this is the same as birth time.
Means time file status was last changed on UNIX.
On Windows NT this is the same as birth time.
This provides some indication of how much physical storage a file
actually consumes. For example, for small file systems, your system
might report this number as being 8, which means 4096 bytes.
This field might be of assistance in computing optimal i/o sizes.
Please note this field has no relationship to blocks, as the latter
is fixed at a 512 byte size.
This can be used to detect some other process used `rename()` to swap
out a file underneath you, so you can do a refresh. redbean does it
during each main process heartbeat for its own use cases.
On Windows NT this is set to `NtByHandleFileInformation::FileIndex`.
On Windows NT this is set to `NtByHandleFileInformation::VolumeSerialNumber`.
This value may be set to `0` or `-1` for files that aren't devices,
depending on the operating system. `unix.major()` and `unix.minor()`
may be used to extract the device numbers.
The unix.Sigset class defines a mutable bitset that may currently
contain 128 entries. See `unix.NSIG` to find out how many signals
your operating system actually supports.
Constructs new signal bitset object.

**Parameters:**

- `sig` (integer)

**Returns:**

- `integer`: bytes Size of file in bytes.
- `integer`: mode
- `integer`: uid User ID of file owner.
- `integer`: gid Group ID of file owner.
- `integer`: unixts, integer nanos
- `integer`: unixts, integer nanos Last modified time.
- `integer`: unixts, integer nanos Last access time.
- `integer`: unixts, integer nanos  Complicated time.
- `integer`: count512 Number of 512-byte blocks used by storage medium.
- `integer`: bytes Block size that underlying device uses.
- `integer`: inode Inode number.
- `integer`: dev ID of device containing file.
- `integer`: rdev Information about device type.
- `any`
- `any`
- `any`
- `unix.Sigset`

### Stat

```lua
unix.Stat
```

It's always the case that `0 ≤ nanos < 1e9`.
On Windows NT this is collected from GetProcessTimes().
It's always the case that `0 ≤ nanos < 1e9`.
On Windows NT this is collected from GetProcessTimes().
On Windows NT this is collected from
`NtProcessMemoryCountersEx::PeakWorkingSetSize / 1024`.
If you chart memory usage over the lifetime of your process, then
this would be the space filled in beneath the chart. The frequency
of kernel scheduling is defined as `unix.CLK_TCK`.  Each time a tick
happens, the kernel samples your process's memory usage, by adding
it to this value. You can derive the average consumption from this
value by computing how many ticks are in `utime + stime`.
Currently only available on FreeBSD and NetBSD.
If you chart memory usage over the lifetime of your process, then
this would be the space filled in beneath the chart. The frequency
of kernel scheduling is defined as unix.CLK_TCK.  Each time a tick
happens, the kernel samples your process's memory usage, by adding
it to this value. You can derive the average consumption from this
value by computing how many ticks are in `utime + stime`.
Currently only available on FreeBSD and NetBSD.
If you chart memory usage over the lifetime of your process, then
this would be the space filled in beneath the chart. The frequency
of kernel scheduling is defined as `unix.CLK_TCK`. Each time a tick
happens, the kernel samples your process's memory usage, by adding
it to this value. You can derive the average consumption from this
value by computing how many ticks are in `utime + stime`.
This is only applicable to redbean if its built with MODE=tiny,
because redbean likes to allocate its own deterministic stack.
Currently only available on FreeBSD and NetBSD.
This number indicates how many times redbean was preempted by the
kernel to `memcpy()` a 4096-byte page. This is one of the tradeoffs
`fork()` entails. This number is usually tinier, when your binaries
are tinier.
Not available on Windows NT.
Returns number of major page faults.
This number indicates how many times redbean was preempted by the
kernel to perform i/o. For example, you might have used `mmap()` to
load a large file into memory lazily.
On Windows NT this is `NtProcessMemoryCountersEx::PageFaultCount`.
Operating systems like to reserve hard disk space to back their RAM
guarantees, like using a gold standard for fiat currency. When your
system is under heavy memory load, swap operations may happen while
redbean is working. This number keeps track of them.
Not available on Linux, Windows NT.
On Windows NT this is `NtIoCounters::ReadOperationCount`.
On Windows NT this is `NtIoCounters::WriteOperationCount`.
Not available on Linux, Windows NT.
Not available on Linux, Windows NT.
Not available on Linux.
This number is a good thing. It means your redbean finished its work
quickly enough within a time slice that it was able to give back the
remaining time to the system.
@return integer count number of non-consensual context switches.
This number is a bad thing. It means your redbean was preempted by a
higher priority process after failing to finish its work, within the
allotted time slice.
`unix.Stat` objects are created by `stat()` or `fstat()`.

### WEXITSTATUS

```lua
unix.WEXITSTATUS(wstatus)
```

Returns code passed to exit() assuming `WIFEXITED(wstatus)` is true.

**Parameters:**

- `wstatus` (integer)

**Returns:**

- `integer`: exitcode uint8

### WIFEXITED

```lua
unix.WIFEXITED(wstatus)
```

Returns `true` if process exited cleanly.

**Parameters:**

- `wstatus` (integer)

**Returns:**

- `boolean`

### WIFSIGNALED

```lua
unix.WIFSIGNALED(wstatus)
```

Returns `true` if process terminated due to a signal.

**Parameters:**

- `wstatus` (integer)

**Returns:**

- `boolean`

### WTERMSIG

```lua
unix.WTERMSIG(wstatus)
```

Returns signal that caused process to terminate assuming
`WIFSIGNALED(wstatus)` is `true`.

**Parameters:**

- `wstatus` (integer)

**Returns:**

- `integer`: sig uint8

### accept

```lua
unix.accept(serverfd, flags)
```

Accepts new client socket descriptor for a listening tcp socket.
`flags` may have any combination (using bitwise OR) of:
- `SOCK_CLOEXEC`
- `SOCK_NONBLOCK`

**Parameters:**

- `serverfd` (integer)
- `flags` (integer?)

**Returns:**

- `integer`: clientfd, uint32 ip, uint16 port

### access

```lua
unix.access(path, how, flags, dirfd)
```

Checks if effective user of current process has permission to access file.
- `AT_SYMLINK_NOFOLLOW`: do not follow symbolic links.

**Parameters:**

- `path` (string)
- `how` (integer): can be `R_OK`, `W_OK`, `X_OK`, or `F_OK` to check for read, write, execute, and existence respectively.
- `flags` (integer) *(optional)*: may have any of:
- `dirfd` (integer) *(optional)*

**Returns:**

- `true`

### bind

```lua
unix.bind(fd, ip, port)
```

Binds socket.
`ip` and `port` are in host endian order. For example, if you
wanted to listen on `1.2.3.4:31337` you could do any of these
unix.bind(sock, 0x01020304, 31337)
unix.bind(sock, ParseIp('1.2.3.4'), 31337)
unix.bind(sock, 1 << 24 | 0 << 16 | 0 << 8 | 1, 31337)
`ip` and `port` both default to zero. The meaning of bind(0, 0)
is to listen on all interfaces with a kernel-assigned ephemeral
port number, that can be retrieved and used as follows:
sock = assert(unix.socket())  -- create ipv4 tcp socket
assert(unix.bind(sock))       -- all interfaces ephemeral port
ip, port = assert(unix.getsockname(sock))
print("listening on ip", FormatIp(ip), "port", port)
assert(unix.listen(sock))
while true do
client, clientip, clientport = assert(unix.accept(sock))
print("got client ip", FormatIp(clientip), "port", clientport)
unix.close(client)
end
Further note that calling `unix.bind(sock)` is equivalent to not
calling bind() at all, since the above behavior is the default.

**Parameters:**

- `fd` (integer)
- `ip` (uint32) *(optional)*
- `port` (uint16) *(optional)*

**Returns:**

- `true`

### chdir

```lua
unix.chdir(path)
```

Changes current directory to `path`.

**Parameters:**

- `path` (string)

**Returns:**

- `true`

### chmod

```lua
unix.chmod(path, mode, flags, dirfd)
```

Changes mode bits on file.
On Windows NT the chmod system call only changes the read-only
status of a file.

**Parameters:**

- `path` (string)
- `mode` (integer)
- `flags` (integer) *(optional)*
- `dirfd` (integer) *(optional)*

**Returns:**

- `true`

### chown

```lua
unix.chown(path, uid, gid, flags, dirfd)
```

Changes user and group on file.
Returns `ENOSYS` on Windows NT.

**Parameters:**

- `path` (string)
- `uid` (integer)
- `gid` (integer)
- `flags` (integer) *(optional)*
- `dirfd` (integer) *(optional)*

**Returns:**

- `true`

### chroot

```lua
unix.chroot(path)
```

Changes root directory.
Returns `ENOSYS` on Windows NT.

**Parameters:**

- `path` (string)

**Returns:**

- `true`

### clock_gettime

```lua
unix.clock_gettime(clock)
```

Returns nanosecond precision timestamp from system, e.g.
>: unix.clock_gettime()
1651137352      774458779
>: Benchmark(unix.clock_gettime)
126     393     571     1
`clock` can be any one of of:
- `CLOCK_REALTIME` returns a wall clock timestamp represented in
nanoseconds since the UNIX epoch (~1970). It'll count time in the
suspend state. This clock is subject to being smeared by various
adjustments made by NTP. These timestamps can have unpredictable
discontinuous jumps when clock_settime() is used. Therefore this
clock is the default clock for everything, even pthread condition
variables. Cosmopoiltan guarantees this clock will never raise
`EINVAL` and also guarantees `CLOCK_REALTIME == 0` will always be
the case. On Windows this maps to GetSystemTimePreciseAsFileTime().
On platforms with vDSOs like Linux, Windows, and MacOS ARM64 this
should take about 20 nanoseconds.
- `CLOCK_MONOTONIC` returns a timestamp with an unspecified epoch,
that should be when the system was powered on. These timestamps
shouldn't go backwards. Timestamps shouldn't count time spent in
the sleep, suspend, and hibernation states. These timestamps won't
be impacted by clock_settime(). These timestamps may be impacted by
frequency adjustments made by NTP. Cosmopoiltan guarantees this
clock will never raise `EINVAL`. MacOS and BSDs use the word
"uptime" to describe this clock. On Windows this maps to
QueryUnbiasedInterruptTimePrecise().
- `CLOCK_BOOTTIME` is a monotonic clock returning a timestamp with an
unspecified epoch, that should be relative to when the host system
was powered on. These timestamps shouldn't go backwards. Timestamps
should also include time spent in a sleep, suspend, or hibernation
state. These timestamps aren't impacted by clock_settime(), but
they may be impacted by frequency adjustments made by NTP. This
clock will raise an `EINVAL` error on extremely old Linux distros
like RHEL5. MacOS and BSDs use the word "monotonic" to describe
this clock. On Windows this maps to QueryInterruptTimePrecise().
- `CLOCK_MONOTONIC_RAW` returns a timestamp from an unspecified
epoch. These timestamps don't count time spent in the sleep,
suspend, and hibernation states. Unlike `CLOCK_MONOTONIC` this
clock is guaranteed to not be impacted by frequency adjustments or
discontinuous jumps caused by clock_settime(). Providing this level
of assurances may make this clock slower than the normal monotonic
clock. Furthermore this clock may cause `EINVAL` to be raised if
running on a host system that doesn't provide those guarantees,
e.g. OpenBSD and MacOS on AMD64.
- `CLOCK_REALTIME_COARSE` is the same as `CLOCK_REALTIME` except
it'll go faster if the host OS provides a cheaper way to read the
wall time. Please be warned that coarse can be really coarse.
Rather than nano precision, you're looking at `CLK_TCK` precision,
which can lag as far as 30 milliseconds behind or possibly more.
Cosmopolitan may fallback to `CLOCK_REALTIME` if a faster less
accurate clock isn't provided by the system. This clock will raise
an `EINVAL` error on extremely old Linux distros like RHEL5.
- `CLOCK_MONOTONIC_COARSE` is the same as `CLOCK_MONOTONIC` except
it'll go faster if the host OS provides a cheaper way to read the
unbiased time. Please be warned that coarse can be really coarse.
Rather than nano precision, you're looking at `CLK_TCK` precision,
which can lag as far as 30 milliseconds behind or possibly more.
Cosmopolitan may fallback to `CLOCK_REALTIME` if a faster less
accurate clock isn't provided by the system. This clock will raise
an `EINVAL` error on extremely old Linux distros like RHEL5.
- `CLOCK_PROCESS_CPUTIME_ID` returns the amount of time this process
was actively scheduled. This is similar to getrusage() and clock().
Cosmopoiltan guarantees this clock will never raise `EINVAL`.
- `CLOCK_THREAD_CPUTIME_ID` returns the amount of time this thread
was actively scheduled. This is similar to getrusage() and clock().
Cosmopoiltan guarantees this clock will never raise `EINVAL`.
Returns `EINVAL` if clock isn't supported on platform.
This function only fails if `clock` is invalid.
This function goes fastest on Linux and Windows.

**Parameters:**

- `clock` (integer) *(optional)*

**Returns:**

- `integer`: seconds, integer nanos

### close

```lua
unix.close(fd)
```

Closes file descriptor.
This function should never be called twice for the same file
descriptor, regardless of whether or not an error happened. The file
descriptor is always gone after close is called. So it technically
always succeeds, but that doesn't mean an error should be ignored.
For example, on NFS a close failure could indicate data loss.
Closing does not mean that scheduled i/o operations have been
completed. You'd need to use fsync() or fdatasync() beforehand to
ensure that. You shouldn't need to do that normally, because our
close implementation guarantees a consistent view, since on systems
where it isn't guaranteed (like Windows) close will implicitly sync.
File descriptors are automatically closed on exit().
Returns `EBADF` if `fd` wasn't valid.
Returns `EINTR` possibly maybe.
Returns `EIO` if an i/o error occurred.

**Parameters:**

- `fd` (integer)

**Returns:**

- `true`

### commandv

```lua
unix.commandv(prog)
```

Performs `$PATH` lookup of executable.
unix = require 'unix'
prog = assert(unix.commandv('ls'))
unix.execve(prog, {prog, '-hal', '.'}, {'PATH=/bin'})
unix.exit(127)
If `prog` is an absolute path, then it's returned as-is. If `prog`
contains slashes then it's not path searched either and will be
returned if it exists. On Windows, it's recommended that you install
programs from cosmos to c:/bin/ without any .exe or .com suffix, so
they can be discovered like they would on UNIX. If you want to find
a program like notepad on the $PATH using this function, then you
need to specify "notepad.exe" so it includes the extension.

**Parameters:**

- `prog` (string)

**Returns:**

- `string`: path

### connect

```lua
unix.connect(fd, ip, port)
```

Connects a TCP socket to a remote host.
With TCP this is a blocking operation. For a UDP socket it simply
remembers the intended address so that `send()` or `write()` may be used
rather than `sendto()`.

**Parameters:**

- `fd` (integer)
- `ip` (uint32)
- `port` (uint16)

**Returns:**

- `true`

### dup

```lua
unix.dup(oldfd, newfd, flags, lowest)
```

Duplicates file descriptor.
`newfd` may be specified to choose a specific number for the new
file descriptor. If it's already open, then the preexisting one will
be silently closed. `EINVAL` is returned if `newfd` equals `oldfd`.
`flags` can have `O_CLOEXEC` which means the returned file
descriptors will be automatically closed upon execve().
`lowest` defaults to zero and defines the lowest numbered file
descriptor that's acceptable to use. If `newfd` is specified then
`lowest` is ignored. For example, if you wanted to duplicate
standard input, then:
stdin2 = assert(unix.dup(0, nil, unix.O_CLOEXEC, 3))
Will ensure that, in the rare event standard output or standard
error are closed, you won't accidentally duplicate standard input to
those numbers.

**Parameters:**

- `oldfd` (integer)
- `newfd` (integer?)
- `flags` (integer?)
- `lowest` (integer?)

**Returns:**

- `integer`: newfd

### environ

```lua
unix.environ()
```

Returns raw environment variables.
This allocates and constructs the C/C++ `environ` variable as a Lua
table consisting of string keys and string values.
This data structure preserves casing. On Windows NT, by convention,
environment variable keys are treated in a case-insensitive way. It
is the responsibility of the caller to consider this.
This data structure preserves valueless variables. It's possible on
both UNIX and Windows to have an environment variable without an
equals, even though it's unusual.
This data structure preserves duplicates. For example, on Windows,
there's some irregular uses of environment variables such as how the
command prompt inserts multiple environment variables with empty
string as keys, for its internal bookkeeping.

**Returns:**

- `table<string,`: string?>

### execve

```lua
unix.execve(prog, args, env)
```

Exits current process, replacing it with a new instance of the
specified program. `prog` needs to be an absolute path, see
commandv(). `env` defaults to to the current `environ`. Here's
a basic usage example:
unix.execve("/bin/ls", {"/bin/ls", "-hal"}, {"PATH=/bin"})
unix.exit(127)
`prog` needs to be the resolved pathname of your executable. You
can use commandv() to search your `PATH`.
`args` is a string list table. The first element in `args`
should be `prog`. Values are coerced to strings. This parameter
defaults to `{prog}`.
`env` is a string list table. Values are coerced to strings. No
ordering requirement is imposed. By convention, each string has its
key and value divided by an equals sign without spaces. If this
parameter is not specified, it'll default to the C/C++ `environ`
variable which is inherited from the shell that launched redbean.
It's the responsibility of the user to supply a sanitized environ
when spawning untrusted processes.
`execve()` is normally called after `fork()` returns `0`. If that isn't
the case, then your redbean worker will be destroyed.
This function never returns on success.
`EAGAIN` is returned if you've enforced a max number of
processes using `setrlimit(RLIMIT_NPROC)`.

**Parameters:**

- `prog` (string)
- `args` (string[])
- `env` (string[])

**Returns:**

- `nil,`: unix.Errno error

### exit

```lua
unix.exit(exitcode)
```

Invokes `_Exit(exitcode)` on the process. This will immediately
halt the current process. Memory will be freed. File descriptors
will be closed. Any open connections it owns will be reset. This
function never returns.

**Parameters:**

- `exitcode` (integer?)

### fcntl

```lua
unix.fcntl(fd, cmd, ...)
```

Manipulates file descriptor.
`cmd` may be one of:
- `unix.F_GETFD` Returns file descriptor flags.
- `unix.F_SETFD` Sets file descriptor flags.
- `unix.F_GETFL` Returns file descriptor status flags.
- `unix.F_SETFL` Sets file descriptor status flags.
- `unix.F_SETLK` Acquires lock on file interval.
- `unix.F_SETLKW` Waits for lock on file interval.
- `unix.F_GETLK` Acquires information about lock.
unix.fcntl(fd:int, unix.F_GETFD)
├─→ flags:int
└─→ nil, unix.Errno
Returns file descriptor flags.
The returned `flags` may include any of:
- `unix.FD_CLOEXEC` if `fd` was opened with `unix.O_CLOEXEC`.
Returns `EBADF` if `fd` isn't open.
unix.fcntl(fd:int, unix.F_SETFD, flags:int)
├─→ true
└─→ nil, unix.Errno
Sets file descriptor flags.
`flags` may include any of:
- `unix.FD_CLOEXEC` to re-open `fd` with `unix.O_CLOEXEC`.
Returns `EBADF` if `fd` isn't open.
unix.fcntl(fd:int, unix.F_GETFL)
├─→ flags:int
└─→ nil, unix.Errno
Returns file descriptor status flags.
`flags & unix.O_ACCMODE` includes one of:
- `O_RDONLY`
- `O_WRONLY`
- `O_RDWR`
Examples of values `flags & ~unix.O_ACCMODE` may include:
- `O_NONBLOCK`
- `O_APPEND`
- `O_SYNC`
- `O_NOATIME` on Linux
- `O_DIRECT` on Linux/FreeBSD/NetBSD/Windows
Examples of values `flags & ~unix.O_ACCMODE` won't include:
- `O_CREAT`
- `O_TRUNC`
- `O_EXCL`
- `O_NOCTTY`
Returns `EBADF` if `fd` isn't open.
unix.fcntl(fd:int, unix.F_SETFL, flags:int)
├─→ true
└─→ nil, unix.Errno
Changes file descriptor status flags.
Examples of values `flags` may include:
- `O_NONBLOCK`
- `O_APPEND`
- `O_SYNC`
- `O_NOATIME` on Linux
- `O_DIRECT` on Linux/FreeBSD/NetBSD/Windows
These values should be ignored:
- `O_RDONLY`, `O_WRONLY`, `O_RDWR`
- `O_CREAT`, `O_TRUNC`, `O_EXCL`
- `O_NOCTTY`
Returns `EBADF` if `fd` isn't open.
unix.fcntl(fd:int, unix.F_SETLK[, type[, start[, len[, whence]]]])
unix.fcntl(fd:int, unix.F_SETLKW[, type[, start[, len[, whence]]]])
├─→ true
└─→ nil, unix.Errno
Acquires lock on file interval.
POSIX Advisory Locks allow multiple processes to leave voluntary
hints to each other about which portions of a file they're using.
The command may be:
- `F_SETLK` to acquire lock if possible
- `F_SETLKW` to wait for lock if necessary
`fd` is file descriptor of open() file.
`type` may be one of:
- `F_RDLCK` for read lock (default)
- `F_WRLCK` for read/write lock
- `F_UNLCK` to unlock
`start` is 0-indexed byte offset into file. The default is zero.
`len` is byte length of interval. Zero is the default and it means
until the end of the file.
`whence` may be one of:
- `SEEK_SET` start from beginning (default)
- `SEEK_CUR` start from current position
- `SEEK_END` start from end
Returns `EAGAIN` if lock couldn't be acquired. POSIX says this
theoretically could also be `EACCES` but we haven't seen this
behavior on any of our supported platforms.
Returns `EBADF` if `fd` wasn't open.
unix.fcntl(fd:int, unix.F_GETLK[, type[, start[, len[, whence]]]])
├─→ unix.F_UNLCK
├─→ type, start, len, whence, pid
└─→ nil, unix.Errno
Acquires information about POSIX advisory lock on file.
This function accepts the same parameters as fcntl(F_SETLK) and
tells you if the lock acquisition would be successful for a given
range of bytes. If locking would have succeeded, then F_UNLCK is
returned. If the lock would not have succeeded, then information
about a conflicting lock is returned.
Returned `type` may be `F_RDLCK` or `F_WRLCK`.
Returned `pid` is the process id of the current lock owner.
This function is currently not supported on Windows.
Returns `EBADF` if `fd` wasn't open.

**Parameters:**

- `fd` (integer)
- `cmd` (integer)

**Returns:**

- `any`: ...

### fdatasync

```lua
unix.fdatasync(fd)
```

These functions are used to make programs slower by asking the
operating system to flush data to the physical medium.

**Parameters:**

- `fd` (integer)

**Returns:**

- `true`

### fdopendir

```lua
unix.fdopendir(fd)
```

Opens directory for listing its contents, via an fd.
@param fd integer should be created by `open(path, O_RDONLY|O_DIRECTORY)`.
The returned `unix.Dir` takes ownership of the file descriptor
and will close it automatically when garbage collected.

**Returns:**

- `function`: next, unix.Dir state

### fork

```lua
unix.fork()
```

Creates a new process mitosis style.
This system call returns twice. The parent process gets the nonzero
pid. The child gets zero.
Here's a simple usage example of creating subprocesses, where we
fork off a child worker from a main process hook callback to do some
independent chores, such as sending an HTTP request back to redbean.
-- as soon as server starts, make a fetch to the server
-- then signal redbean to shutdown when fetch is complete
local onServerStart = function()
if assert(unix.fork()) == 0 then
local ok, headers, body = Fetch('http://127.0.0.1:8080/test')
unix.kill(unix.getppid(), unix.SIGTERM)
unix.exit(0)
end
end
OnServerStart = onServerStart
We didn't need to use `wait()` here, because (a) we want redbean to go
back to what it was doing before as the `Fetch()` completes, and (b)
redbean's main process already has a zombie collector. However it's
a moot point, since once the fetch is done, the child process then
asks redbean to gracefully shutdown by sending SIGTERM its parent.
This is actually a situation where we *must* use fork, because the
purpose of the main redbean process is to call accept() and create
workers. So if we programmed redbean to use the main process to send
a blocking request to itself instead, then redbean would deadlock
and never be able to accept() the client.
While deadlocking is an extreme example, the truth is that latency
issues can crop up for the same reason that just cause jitter
instead, and as such, can easily go unnoticed. For example, if you
do soemething that takes longer than a few milliseconds from inside
your redbean heartbeat, then that's a few milliseconds in which
redbean is no longer concurrent, and tail latency is being added to
its ability to accept new connections. fork() does a great job at
solving this.
If you're not sure how long something will take, then when in doubt,
fork off a process. You can then report its completion to something
like SQLite. Redbean makes having lots of processes cheap. On Linux
they're about as lightweight as what heavyweight environments call
greenlets. You can easily have 10,000 Redbean workers on one PC.
Here's some benchmarks for fork() performance across platforms:
Linux 5.4 fork      l:     97,200𝑐    31,395𝑛𝑠  [metal]
FreeBSD 12 fork     l:    236,089𝑐    78,841𝑛𝑠  [vmware]
Darwin 20.6 fork    l:    295,325𝑐    81,738𝑛𝑠  [metal]
NetBSD 9 fork       l:  5,832,027𝑐 1,947,899𝑛𝑠  [vmware]
OpenBSD 6.8 fork    l: 13,241,940𝑐 4,422,103𝑛𝑠  [vmware]
Windows10 fork      l: 18,802,239𝑐 6,360,271𝑛𝑠  [metal]
One of the benefits of using `fork()` is it creates an isolation
barrier between the different parts of your app. This can lead to
enhanced reliability and security. For example, redbean uses fork so
it can wipe your ssl keys from memory before handing over control to
request handlers that process untrusted input. It also ensures that
if your Lua app crashes, it won't take down the server as a whole.
Hence it should come as no surprise that `fork()` would go slower on
operating systems that have more security features. So depending on
your use case, you can choose the operating system that suits you.

**Returns:**

- `integer|0`: childpid

### fstat

```lua
unix.fstat(fd)
```

Gets information about opened file descriptor.
`flags` may have any of:
- `AT_SYMLINK_NOFOLLOW`: do not follow symbolic links.
`dirfd` defaults to to `unix.AT_FDCWD` and may optionally be set to
a directory file descriptor to which `path` is relative.
A common use for `fstat()` is getting the size of a file. For example:
fd = assert(unix.open("hello.txt", unix.O_RDONLY))
st = assert(unix.fstat(fd))
Log(kLogInfo, 'hello.txt is %d bytes in size' % {st:size()})
unix.close(fd)

**Parameters:**

- `fd` (integer): should be a file descriptor that was opened using `unix.open(path, O_RDONLY|O_DIRECTORY)`.

**Returns:**

- `unix.Stat`

### fsync

```lua
unix.fsync(fd)
```

These functions are used to make programs slower by asking the
operating system to flush data to the physical medium.

**Parameters:**

- `fd` (integer)

**Returns:**

- `true`

### ftruncate

```lua
unix.ftruncate(fd, length)
```

Reduces or extends underlying physical medium of open file.
If file was originally larger, content >length is lost.

**Parameters:**

- `fd` (integer)
- `length` (integer) *(optional)*: defaults to zero (`0`)

**Returns:**

- `true`

### futimens

```lua
unix.futimens(fd, asecs, ananos, msecs, mnanos)
```

Changes access and/or modified timestamps on file descriptor.
`fd` is the file descriptor of a file opened with `unix.open`.
The `asecs` and `ananos` parameters set the access time. If they're
none or nil, the current time will be used.
The `msecs` and `mnanos` parameters set the modified time. If
they're none or nil, the current time will be used.
The nanosecond parameters (`ananos` and `mnanos`) must be on the
interval [0,1000000000) or `unix.EINVAL` is raised. On XNU this is
truncated to microsecond precision. On Windows NT, it's truncated to
hectonanosecond precision. These nanosecond parameters may also be
set to one of the following special values:
- `unix.UTIME_NOW`: Fill this timestamp with current time.
- `unix.UTIME_OMIT`: Do not alter this timestamp.
This system call is currently not available on very old versions of
Linux, e.g. RHEL5.

**Parameters:**

- `fd` (integer)
- `asecs` (integer)
- `ananos` (integer)
- `msecs` (integer)
- `mnanos` (integer)

**Returns:**

- `0`

### getcwd

```lua
unix.getcwd()
```

Returns current working directory.
On Windows NT, this function transliterates `\` to `/` and
furthermore prefixes `//?/` to WIN32 DOS-style absolute paths,
thereby assisting with simple absolute filename checks in addition
to enabling one to exceed the traditional 260 character limit.

**Returns:**

- `string`: path

### getegid

```lua
unix.getegid()
```

Gets effective group id.
On Windows this system call is polyfilled as getuid().
This function does not fail.

**Returns:**

- `integer`: gid

### geteuid

```lua
unix.geteuid()
```

Gets effective user id.
For example, if your redbean is a setuid binary, then getuid() will
return the uid of the user running the program, and geteuid() shall
return zero which means root, assuming that's the file owning user.
On Windows this system call is polyfilled as getuid().
This function does not fail.

**Returns:**

- `integer`: uid

### getgid

```lua
unix.getgid()
```

Sets real group id.
On Windows this system call is polyfilled as getuid().
This function does not fail.

**Returns:**

- `integer`: gid

### gethostname

```lua
unix.gethostname()
```

Returns hostname of system.

**Returns:**

- `string`: host

### getpeername

```lua
unix.getpeername(fd)
```

Retrieves the remote address of a socket.
This operation will either fail on `AF_UNIX` sockets or return an
empty string.

**Parameters:**

- `fd` (integer)

**Returns:**

- `uint32`: ip, uint16 port

### getpgid

```lua
unix.getpgid(pid)
```

Gets process group id the modern way.

**Parameters:**

- `pid` (integer)

### getpgrp

```lua
unix.getpgrp()
```

Gets process group id.

**Returns:**

- `integer`: pgid

### getpid

```lua
unix.getpid()
```

Returns process id of current process.
This function does not fail.

**Returns:**

- `integer`: pid

### getppid

```lua
unix.getppid()
```

Returns process id of parent process.
This function does not fail.

**Returns:**

- `integer`: pid

### getrlimit

```lua
unix.getrlimit(resource)
```

Returns information about resource limits for current process.

**Parameters:**

- `resource` (integer)

**Returns:**

- `integer`: soft, integer hard

### getrusage

```lua
unix.getrusage(who)
```

Returns information about resource usage for current process, e.g.
>: unix.getrusage()
{utime={0, 53644000}, maxrss=44896, minflt=545, oublock=24, nvcsw=9}
- `RUSAGE_SELF`: current process
- `RUSAGE_THREAD`: current thread
- `RUSAGE_CHILDREN`: not supported on Windows NT
- `RUSAGE_BOTH`: not supported on non-Linux

**Parameters:**

- `who` (integer) *(optional)*: defaults to `RUSAGE_SELF` and can be any of:

**Returns:**

- `unix.Rusage`: # See `unix.Rusage` for details on returned fields.

### getsid

```lua
unix.getsid(pid)
```

Gets session id.

**Parameters:**

- `pid` (integer)

**Returns:**

- `integer`: sid

### getsockname

```lua
unix.getsockname(fd)
```

Retrieves the local address of a socket.

**Parameters:**

- `fd` (integer)

**Returns:**

- `uint32`: ip, uint16 port

### getsockopt

```lua
unix.getsockopt(fd, level, optname)
```

Tunes networking parameters.
`level` and `optname` may be one of the following pairs. The ellipses
type signature above changes depending on which options are used.
`optname` is the option feature magic number. The constants for
these will be set to `0` if the option isn't supported on the host
platform.
Raises `ENOPROTOOPT` if your `level` / `optname` combination isn't
valid, recognized, or supported on the host platform.
Raises `ENOTSOCK` if `fd` is valid but isn't a socket.
Raises `EBADF` if `fd` isn't valid.
unix.getsockopt(fd:int, level:int, optname:int)
├─→ value:int
└─→ nil, unix.Errno
unix.setsockopt(fd:int, level:int, optname:int, value:bool)
├─→ true
└─→ nil, unix.Errno
- `SOL_SOCKET`, `SO_TYPE`
- `SOL_SOCKET`, `SO_DEBUG`
- `SOL_SOCKET`, `SO_ACCEPTCONN`
- `SOL_SOCKET`, `SO_BROADCAST`
- `SOL_SOCKET`, `SO_REUSEADDR`
- `SOL_SOCKET`, `SO_REUSEPORT`
- `SOL_SOCKET`, `SO_KEEPALIVE`
- `SOL_SOCKET`, `SO_DONTROUTE`
- `SOL_TCP`, `TCP_NODELAY`
- `SOL_TCP`, `TCP_CORK`
- `SOL_TCP`, `TCP_QUICKACK`
- `SOL_TCP`, `TCP_FASTOPEN_CONNECT`
- `SOL_TCP`, `TCP_DEFER_ACCEPT`
- `SOL_IP`, `IP_HDRINCL`
unix.getsockopt(fd:int, level:int, optname:int)
├─→ value:int
└─→ nil, unix.Errno
unix.setsockopt(fd:int, level:int, optname:int, value:int)
├─→ true
└─→ nil, unix.Errno
- `SOL_SOCKET`, `SO_SNDBUF`
- `SOL_SOCKET`, `SO_RCVBUF`
- `SOL_SOCKET`, `SO_RCVLOWAT`
- `SOL_SOCKET`, `SO_SNDLOWAT`
- `SOL_TCP`, `TCP_KEEPIDLE`
- `SOL_TCP`, `TCP_KEEPINTVL`
- `SOL_TCP`, `TCP_FASTOPEN`
- `SOL_TCP`, `TCP_KEEPCNT`
- `SOL_TCP`, `TCP_MAXSEG`
- `SOL_TCP`, `TCP_SYNCNT`
- `SOL_TCP`, `TCP_NOTSENT_LOWAT`
- `SOL_TCP`, `TCP_WINDOW_CLAMP`
- `SOL_IP`, `IP_TOS`
- `SOL_IP`, `IP_MTU`
- `SOL_IP`, `IP_TTL`
unix.getsockopt(fd:int, level:int, optname:int)
├─→ secs:int, nsecs:int
└─→ nil, unix.Errno
unix.setsockopt(fd:int, level:int, optname:int, secs:int[, nanos:int])
├─→ true
└─→ nil, unix.Errno
- `SOL_SOCKET`, `SO_RCVTIMEO`: If this option is specified then
your stream socket will have a read() / recv() timeout. If the
specified interval elapses without receiving data, then EAGAIN
shall be returned by read. If this option is used on listening
sockets, it'll be inherited by accepted sockets. Your redbean
already does this for GetClientFd() based on the `-t` flag.
- `SOL_SOCKET`, `SO_SNDTIMEO`: This is the same as `SO_RCVTIMEO`
but it applies to the write() / send() functions.
unix.getsockopt(fd:int, unix.SOL_SOCKET, unix.SO_LINGER)
├─→ seconds:int, enabled:bool
└─→ nil, unix.Errno
unix.setsockopt(fd:int, unix.SOL_SOCKET, unix.SO_LINGER, secs:int, enabled:bool)
├─→ true
└─→ nil, unix.Errno
This `SO_LINGER` parameter can be used to make close() a blocking
call. Normally when the kernel returns immediately when it receives
close(). Sometimes it's desirable to have extra assurance on errors
happened, even if it comes at the cost of performance.
unix.setsockopt(serverfd:int, unix.SOL_TCP, unix.TCP_SAVE_SYN, enabled:int)
├─→ true
└─→ nil, unix.Errno
unix.getsockopt(clientfd:int, unix.SOL_TCP, unix.TCP_SAVED_SYN)
├─→ syn_packet_bytes:str
└─→ nil, unix.Errno
This `TCP_SAVED_SYN` option may be used to retrieve the bytes of the
TCP SYN packet that the client sent when the connection for `fd` was
opened. In order for this to work, `TCP_SAVE_SYN` must have been set
earlier on the listening socket. This is Linux-only. You can use the
`OnServerListen` hook to enable SYN saving in your Redbean. When the
`TCP_SAVE_SYN` option isn't used, this may return empty string.

**Parameters:**

- `fd` (integer)
- `level` (integer)
- `optname` (integer)

**Returns:**

- `integer`: value

### getuid

```lua
unix.getuid()
```

Gets real user id.
On Windows this system call is polyfilled by running `GetUserNameW()`
through Knuth's multiplicative hash.
This function does not fail.

**Returns:**

- `integer`: uid

### gmtime

```lua
unix.gmtime(unixts)
```

Breaks down UNIX timestamp into Zulu Time numbers.

**Parameters:**

- `unixts` (integer)

**Returns:**

- `integer`: year
- `integer`: mon 1 ≤ mon ≤ 12
- `integer`: mday 1 ≤ mday ≤ 31
- `integer`: hour 0 ≤ hour ≤ 23
- `integer`: min 0 ≤ min ≤ 59
- `integer`: sec 0 ≤ sec ≤ 60
- `integer`: gmtoffsec ±93600 seconds
- `integer`: wday 0 ≤ wday ≤ 6
- `integer`: yday 0 ≤ yday ≤ 365
- `integer`: dst 1 if daylight savings, 0 if not, -1 if unknown
- `string`: zone

### isatty

```lua
unix.isatty(fd)
```

Returns true if file descriptor is a teletypewriter. Otherwise nil
with an Errno object holding one of the following values:
- `ENOTTY` if `fd` is valid but not a teletypewriter
- `EBADF` if `fd` isn't a valid file descriptor.
- `EPERM` if pledge() is used without `tty` in lenient mode
No other error numbers are possible.

**Parameters:**

- `fd` (integer)

**Returns:**

- `true`

### kill

```lua
unix.kill(pid, sig)
```

Sends signal to process(es).
The impact of this action can be terminating the process, or
interrupting it to request something happen.
`pid` can be:
- `pid > 0` signals one process by id
- `== 0`    signals all processes in current process group
- `-1`      signals all processes possible (except init)
- `< -1`    signals all processes in -pid process group
`sig` can be:
- `0`       checks both if pid exists and we can signal it
- `SIGINT`  sends ctrl-c keyboard interrupt
- `SIGQUIT` sends backtrace and exit signal
- `SIGTERM` sends shutdown signal
- etc.
Windows NT only supports the kill() signals required by the ANSI C89
standard, which are `SIGINT` and `SIGQUIT`. All other signals on the
Windows platform that are sent to another process via kill() will be
treated like `SIGKILL`.

**Parameters:**

- `pid` (integer)
- `sig` (integer)

**Returns:**

- `true`

### link

```lua
unix.link(existingpath, newpath, flags, olddirfd, newdirfd)
```

Creates hard link, so your underlying inode has two names.

**Parameters:**

- `existingpath` (string)
- `newpath` (string)
- `flags` (integer)
- `olddirfd` (integer)
- `newdirfd` (integer)

**Returns:**

- `true`

### listen

```lua
unix.listen(fd, backlog)
```

Begins listening for incoming connections on a socket.

**Parameters:**

- `fd` (integer)
- `backlog` (integer?)

**Returns:**

- `true`

### localtime

```lua
unix.localtime(unixts)
```

Breaks down UNIX timestamp into local time numbers, e.g.
>: unix.localtime(unix.clock_gettime())
2022    4       28      2       14      22      -25200  4       117     1       "PDT"
This follows the same API as `gmtime()` which has further details.
Your redbean ships with a subset of the time zone database.
- `/zip/usr/share/zoneinfo/Honolulu`   Z-10
- `/zip/usr/share/zoneinfo/Anchorage`  Z -9
- `/zip/usr/share/zoneinfo/GST`        Z -8
- `/zip/usr/share/zoneinfo/Boulder`    Z -6
- `/zip/usr/share/zoneinfo/Chicago`    Z -5
- `/zip/usr/share/zoneinfo/New_York`   Z -4
- `/zip/usr/share/zoneinfo/UTC`        Z +0
- `/zip/usr/share/zoneinfo/GMT`        Z +0
- `/zip/usr/share/zoneinfo/London`     Z +1
- `/zip/usr/share/zoneinfo/Berlin`     Z +2
- `/zip/usr/share/zoneinfo/Israel`     Z +3
- `/zip/usr/share/zoneinfo/India`      Z +5
- `/zip/usr/share/zoneinfo/Beijing`    Z +8
- `/zip/usr/share/zoneinfo/Japan`      Z +9
- `/zip/usr/share/zoneinfo/Sydney`     Z+10
You can control which timezone is used using the `TZ` environment
variable. If your time zone isn't included in the above list, you
can simply copy it inside your redbean. The same is also the case
for future updates to the database, which can be swapped out when
needed, without having to recompile.

**Parameters:**

- `unixts` (integer)

**Returns:**

- `integer`: year
- `integer`: mon 1 ≤ mon ≤ 12
- `integer`: mday 1 ≤ mday ≤ 31
- `integer`: hour 0 ≤ hour ≤ 23
- `integer`: min 0 ≤ min ≤ 59
- `integer`: sec 0 ≤ sec ≤ 60
- `integer`: gmtoffsec ±93600 seconds
- `integer`: wday 0 ≤ wday ≤ 6
- `integer`: yday 0 ≤ yday ≤ 365
- `integer`: dst 1 if daylight savings, 0 if not, -1 if unknown
- `string`: zone

### lseek

```lua
unix.lseek(fd, offset, whence)
```

Seeks to file position.
`whence` can be one of:
- `SEEK_SET`: Sets the file position to `offset` [default]
- `SEEK_CUR`: Sets the file position to `position + offset`
- `SEEK_END`: Sets the file position to `filesize + offset`
Returns the new position relative to the start of the file.

**Parameters:**

- `fd` (integer)
- `offset` (integer)
- `whence` (integer) *(optional)*

**Returns:**

- `integer`: newposbytes

### makedirs

```lua
unix.makedirs(path, mode)
```

Unlike mkdir() this convenience wrapper will automatically create
parent parent directories as needed. If the directory already exists
then, unlike mkdir() which returns EEXIST, the makedirs() function
will return success.
`path` is the path of the directory you wish to create.
`mode` is octal permission bits, e.g. `0755`.

**Parameters:**

- `path` (string)
- `mode` (integer) *(optional)*

**Returns:**

- `true`

### mapshared

```lua
unix.mapshared(size)
```

Creates interprocess shared memory mapping.
This function allocates special memory that'll be inherited across
fork in a shared way. By default all memory in Redbean is "private"
memory that's only viewable and editable to the process that owns
it. When unix.fork() happens, memory is copied appropriately so
that changes to memory made in the child process, don't clobber
the memory at those same addresses in the parent process. If you
don't want that to happen, and you want the memory to be shared
similar to how it would be shared if you were using threads, then
you can use this function to achieve just that.
The memory object this function returns may be accessed using its
methods, which support atomics and futexes. It's very low-level.
For example, you can use it to implement scalable mutexes:
mem = unix.mapshared(8000 * 8)
LOCK = 0 -- pick an arbitrary word index for lock
-- From Futexes Are Tricky Version 1.1 § Mutex, Take 3;
-- Ulrich Drepper, Red Hat Incorporated, June 27, 2004.
function Lock()
local ok, old = mem:cmpxchg(LOCK, 0, 1)
if not ok then
if old == 1 then
old = mem:xchg(LOCK, 2)
end
while old > 0 do
mem:wait(LOCK, 2)
old = mem:xchg(LOCK, 2)
end
end
end
function Unlock()
old = mem:add(LOCK, -1)
if old == 2 then
mem:store(LOCK, 0)
mem:wake(LOCK, 1)
end
end
It's possible to accomplish the same thing as unix.mapshared()
using files and unix.fcntl() advisory locks. However this goes
significantly faster. For example, that's what SQLite does and
we recommend using SQLite for IPC in redbean. But, if your app
has thousands of forked processes fighting for a file lock you
might need something lower level than file locks, to implement
things like throttling. Shared memory is a good way to do that
since there's nothing that's faster.
The `size` parameter needs to be a multiple of 8. The returned
memory is zero initialized. When allocating shared memory, you
should try to get as much use out of it as possible, since the
overhead of allocating a single shared mapping is 500 words of
resident memory and 8000 words of virtual memory. It's because
the Cosmopolitan Libc mmap() granularity is 2**16.
This system call does not fail. An exception is instead thrown
if sufficient memory isn't available.

**Parameters:**

- `size` (integer)

**Returns:**

- `unix.Memory`

### mkdir

```lua
unix.mkdir(path, mode, dirfd)
```

Makes directory.
`path` is the path of the directory you wish to create.
`mode` is octal permission bits, e.g. `0755`.
Fails with `EEXIST` if `path` already exists, whether it be a
directory or a file.
Fails with `ENOENT` if the parent directory of the directory you
want to create doesn't exist. For making `a/really/long/path/`
consider using makedirs() instead.
Fails with `ENOTDIR` if a parent directory component existed that
wasn't a directory.
Fails with `EACCES` if the parent directory doesn't grant write
permission to the current user.
Fails with `ENAMETOOLONG` if the path is too long.

**Parameters:**

- `path` (string)
- `mode` (integer) *(optional)*
- `dirfd` (integer) *(optional)*

**Returns:**

- `true`

### nanosleep

```lua
unix.nanosleep(seconds, nanos)
```

Sleeps with nanosecond precision.
Returns `EINTR` if a signal was received while waiting.

**Parameters:**

- `seconds` (integer)
- `nanos` (integer?)

**Returns:**

- `integer`: remseconds, integer remnanos

### open

```lua
unix.open(path, flags, mode, dirfd)
```

Opens file.
Returns a file descriptor integer that needs to be closed, e.g.
fd = assert(unix.open("/etc/passwd", unix.O_RDONLY))
print(unix.read(fd))
unix.close(fd)
`flags` should have one of:
- `O_RDONLY`:     open for reading (default)
- `O_WRONLY`:     open for writing
- `O_RDWR`:       open for reading and writing
The following values may also be OR'd into `flags`:
- `O_CREAT`      create file if it doesn't exist
- `O_TRUNC`      automatic ftruncate(fd,0) if exists
- `O_CLOEXEC`    automatic close() upon execve()
- `O_EXCL`       exclusive access (see below)
- `O_APPEND`     open file for append only
- `O_NONBLOCK`   asks read/write to fail with EAGAIN rather than block
- `O_DIRECTORY`  useful for stat'ing (hint on UNIX but required on NT)
- `O_NOFOLLOW`   fail if it's a symlink (zero on Windows)
- `O_UNLINK`     automatically delete file upon close()
- `O_SYNC`       makes file operations synchronize appropriately
- `O_RSYNC`      synchronize read() operations
- `O_DSYNC`      synchronize write() operations
- `O_DIRECT`     it's complicated (not supported on Apple and OpenBSD)
- `O_NOATIME`    don't record access time (zero on non-Linux)
There are three regular combinations for the above flags:
- `O_RDONLY`: Opens existing file for reading. If it doesn't exist
then nil is returned and errno will be `ENOENT` (or in some other
cases `ENOTDIR`).
- `O_WRONLY|O_CREAT|O_TRUNC`: Creates file. If it already exists,
then the existing copy is destroyed and the opened file will
start off with a length of zero. This is the behavior of the
traditional creat() system call.
- `O_WRONLY|O_CREAT|O_EXCL`: Create file only if doesn't exist
already. If it does exist then `nil` is returned along with
`errno` set to `EEXIST`.
`dirfd` defaults to to `unix.AT_FDCWD` and may optionally be set to
a directory file descriptor to which `path` is relative.
Returns `ENOENT` if `path` doesn't exist.
Returns `ENOTDIR` if `path` contained a directory component that
wasn't a directory
.

**Parameters:**

- `path` (string)
- `flags` (integer)
- `mode` (integer?)
- `dirfd` (integer?)

**Returns:**

- `integer`: fd

### opendir

```lua
unix.opendir(path)
```

Opens directory for listing its contents.
For example, to print a simple directory listing:
Write('<ul>\r\n')
for name, kind, ino, off in assert(unix.opendir(dir)) do
if name ~= '.' and name ~= '..' then
Write('<li>%s\r\n' % {EscapeHtml(name)})
end
end
Write('</ul>\r\n')

**Parameters:**

- `path` (string)

**Returns:**

- `unix.Dir`: state

### pipe

```lua
unix.pipe(flags)
```

Creates fifo which enables communication between processes.
- `O_CLOEXEC`: Automatically close file descriptor upon execve()
- `O_NONBLOCK`: Request `EAGAIN` be raised rather than blocking
- `O_DIRECT`: Enable packet mode w/ atomic reads and writes, so long
as they're no larger than `PIPE_BUF` (guaranteed to be 512+ bytes)
with support limited to Linux, Windows NT, FreeBSD, and NetBSD.
Returns two file descriptors: one for reading and one for writing.
Here's an example of how pipe(), fork(), dup(), etc. may be used
to serve an HTTP response containing the output of a subprocess.
local unix = require "unix"
ls = assert(unix.commandv("ls"))
reader, writer = assert(unix.pipe())
if assert(unix.fork()) == 0 then
unix.close(1)
unix.dup(writer)
unix.close(writer)
unix.close(reader)
unix.execve(ls, {ls, "-Shal"})
unix.exit(127)
else
unix.close(writer)
SetHeader('Content-Type', 'text/plain')
while true do
data, err = unix.read(reader)
if data then
if data ~= "" then
Write(data)
else
break
end
elseif err:errno() ~= EINTR then
Log(kLogWarn, tostring(err))
break
end
end
assert(unix.close(reader))
assert(unix.wait())
end

**Parameters:**

- `flags` (integer?): may have any combination (using bitwise OR) of:

**Returns:**

- `integer`: reader, integer writer

### pledge

```lua
unix.pledge(promises, execpromises, mode)
```

Restrict system operations.
This can be used to sandbox your redbean workers. It allows finer
customization compared to the `-S` flag.
Pledging causes most system calls to become unavailable. On Linux the
disabled calls will return EPERM whereas OpenBSD kills the process.
Using pledge is irreversible. On Linux it causes PR_SET_NO_NEW_PRIVS
to be set on your process.
By default exit and exit_group are always allowed. This is useful
for processes that perform pure computation and interface with the
parent via shared memory.
Once pledge is in effect, the chmod functions (if allowed) will not
permit the sticky/setuid/setgid bits to change. Linux will EPERM here
and OpenBSD should ignore those three bits rather than crashing.
User and group IDs also can't be changed once pledge is in effect.
OpenBSD should ignore the chown functions without crashing. Linux
will just EPERM.
Memory functions won't permit creating executable code after pledge.
Restrictions on origin of SYSCALL instructions will become enforced
on Linux (cf. msyscall) after pledge too, which means the process
gets killed if SYSCALL is used outside the .privileged section. One
exception is if the "exec" group is specified, in which case these
restrictions need to be loosened.
This list has been curated to focus on the
system calls for which this module provides wrappers. See the
Cosmopolitan Libc pledge() documentation for a comprehensive and
authoritative list of raw system calls. Having the raw system call
list may be useful if you're executing foreign programs.
### stdio
Allows read, write, send, recv, recvfrom, close, clock_getres,
clock_gettime, dup, fchdir, fstat, fsync, fdatasync, ftruncate,
getdents, getegid, getrandom, geteuid, getgid, getgroups,
getitimer, getpgid, getpgrp, getpid, hgetppid, getresgid,
getresuid, getrlimit, getsid, gettimeofday, getuid, lseek,
madvise, brk, mmap/mprotect (PROT_EXEC isn't allowed), msync,
munmap, gethostname, nanosleep, pipe, pipe2, poll, setitimer,
shutdown, sigaction, sigsuspend, sigprocmask, socketpair, umask,
wait4, getrusage, ioctl(FIONREAD), ioctl(FIONBIO), ioctl(FIOCLEX),
ioctl(FIONCLEX), fcntl(F_GETFD), fcntl(F_SETFD), fcntl(F_GETFL),
fcntl(F_SETFL).
### rpath
Allows chdir, getcwd, open, stat, fstat, access, readlink, chmod,
chmod, fchmod.
### wpath
Allows getcwd, open, stat, fstat, access, readlink, chmod, fchmod.
### cpath
Allows rename, link, symlink, unlink, mkdir, rmdir.
### fattr
Allows chmod, fchmod, utimensat, futimens.
### flock
Allows flock, fcntl(F_GETLK), fcntl(F_SETLK), fcntl(F_SETLKW).
### tty
Allows isatty, tiocgwinsz, tcgets, tcsets, tcsetsw, tcsetsf.
### inet
Allows socket (AF_INET), listen, bind, connect, accept,
getpeername, getsockname, setsockopt, getsockopt.
### unix
Allows socket (AF_UNIX), listen, bind, connect, accept,
getpeername, getsockname, setsockopt, getsockopt.
### dns
Allows sendto, recvfrom, socket(AF_INET), connect.
### recvfd
Allows recvmsg, recvmmsg.
### sendfd
Allows sendmsg, sendmmsg.
### proc
Allows fork, vfork, clone, kill, tgkill, getpriority, setpriority,
setrlimit, setpgid, setsid.
### id
Allows setuid, setreuid, setresuid, setgid, setregid, setresgid,
setgroups, setrlimit, getpriority, setpriority.
### settime
Allows settimeofday and clock_adjtime.
### unveil
Allows unveil().
### exec
Allows execve.
If the executable in question needs a loader, then you will need
"rpath prot_exec" too. With APE, security is strongest when you
assimilate your binaries beforehand, using the --assimilate flag,
or the o//tool/build/assimilate program. On OpenBSD this is
mandatory.
### prot_exec
Allows mmap(PROT_EXEC) and mprotect(PROT_EXEC).
This may be needed to launch non-static non-native executables,
such as non-assimilated APE binaries, or programs that link
dynamic shared objects, i.e. most Linux distro binaries.
In that case, this specifies the promises that'll apply once `execve()`
happens. If this is `NULL` then the default is used, which is
unrestricted. OpenBSD allows child processes to escape the sandbox
(so a pledged OpenSSH server process can do things like spawn a root
shell). Linux however requires monotonically decreasing privileges.
This function will will perform some validation on Linux to make
sure that `execpromises` is a subset of `promises`. Your libc
wrapper for `execve()` will then apply its SECCOMP BPF filter later.
Since Linux has to do this before calling `sys_execve()`, the executed
process will be weakened to have execute permissions too.
- `unix.PLEDGE_PENALTY_KILL_THREAD` causes the violating thread to
be killed. This is the default on Linux. It's effectively the
same as killing the process, since redbean has no threads. The
termination signal can't be caught and will be either `SIGSYS`
or `SIGABRT`. Consider enabling stderr logging below so you'll
know why your program failed. Otherwise check the system log.
- `unix.PLEDGE_PENALTY_KILL_PROCESS` causes the process and all
its threads to be killed. This is always the case on OpenBSD.
- `unix.PLEDGE_PENALTY_RETURN_EPERM` causes system calls to just
return an `EPERM` error instead of killing. This is a gentler
solution that allows code to display a friendly warning. Please
note this may lead to weird behaviors if the software being
sandboxed is lazy about checking error results.
`mode` may optionally bitwise or the following flags:
- `unix.PLEDGE_STDERR_LOGGING` enables friendly error message
logging letting you know which promises are needed whenever
violations occur. Without this, violations will be logged to
`dmesg` on Linux if the penalty is to kill the process. You
would then need to manually look up the system call number and
then cross reference it with the cosmopolitan libc pledge()
documentation. You can also use `strace -ff` which is easier.
This is ignored OpenBSD, which already has a good system log.
Turning on stderr logging (which uses SECCOMP trapping) also
means that the `unix.WTERMSIG()` on your killed processes will
always be `unix.SIGABRT` on both Linux and OpenBSD. Otherwise,
Linux prefers to raise `unix.SIGSYS`.

**Parameters:**

- `promises` (string) *(optional)*: may include any of the following groups delimited by spaces.
- `execpromises` (string) *(optional)*: only matters if "exec" is specified in `promises`.
- `mode` (integer?): if specified should specify one penalty:

**Returns:**

- `true`

### poll

```lua
unix.poll(fds, timeoutms)
```

Checks for events on a set of file descriptors.
The table of file descriptors to poll uses sparse integer keys. Any
pairs with non-integer keys will be ignored. Pairs with negative
keys are ignored by poll(). The returned table will be a subset of
the supplied file descriptors.
`events` and `revents` may be any combination (using bitwise OR) of:
- `POLLIN` (events, revents): There is data to read.
- `POLLOUT` (events, revents): Writing is now possible, although may
still block if available space in a socket or pipe is exceeded
(unless `O_NONBLOCK` is set).
- `POLLPRI` (events, revents): There is some exceptional condition
(for example, out-of-band data on a TCP socket).
- `POLLRDHUP` (events, revents): Stream socket peer closed
connection, or shut down writing half of connection.
- `POLLERR` (revents): Some error condition.
- `POLLHUP` (revents): Hang up. When reading from a channel such as
a pipe or a stream socket, this event merely indicates that the
peer closed its end of the channel.
- `POLLNVAL` (revents): Invalid request.
If this is set to -1 then that means block as long as it takes until there's an
event or an interrupt. If the timeout expires, an empty table is returned.

**Parameters:**

- `fds` (table<integer,integer>): `{[fd:int]=events:int, ...}`
- `timeoutms` (integer?): the number of milliseconds to block.

**Returns:**

- `table<integer,integer>`: `{[fd:int]=revents:int, ...}`

### raise

```lua
unix.raise(sig)
```

Triggers signal in current process.
This is pretty much the same as `kill(getpid(), sig)`.

**Parameters:**

- `sig` (integer)

**Returns:**

- `integer`: rc

### read

```lua
unix.read(fd, bufsiz, offset)
```

Reads from file descriptor.
This function returns empty string on end of file. The exception is
if `bufsiz` is zero, in which case an empty returned string means
the file descriptor works.

**Parameters:**

- `fd` (integer)
- `bufsiz` (string?)
- `offset` (integer?)

**Returns:**

- `string`: data

### readlink

```lua
unix.readlink(path, dirfd)
```

Reads contents of symbolic link.
Note that broken links are supported on all platforms. A symbolic
link can contain just about anything. It's important to not assume
that `content` will be a valid filename.
On Windows NT, this function transliterates `\` to `/` and
furthermore prefixes `//?/` to WIN32 DOS-style absolute paths,
thereby assisting with simple absolute filename checks in addition
to enabling one to exceed the traditional 260 character limit.

**Parameters:**

- `path` (string)
- `dirfd` (integer) *(optional)*

**Returns:**

- `string`: content

### realpath

```lua
unix.realpath(path)
```

Returns absolute path of filename, with `.` and `..` components
removed, and symlinks will be resolved.

**Parameters:**

- `path` (string)

**Returns:**

- `string`: path

### recv

```lua
unix.recv(fd, bufsiz, flags)
```

- `MSG_WAITALL`
- `MSG_DONTROUTE`
- `MSG_PEEK`
- `MSG_OOB`

**Parameters:**

- `fd` (integer)
- `bufsiz` (integer?)
- `flags` (integer?): may have any combination (using bitwise OR) of:

**Returns:**

- `string`: data

### recvfrom

```lua
unix.recvfrom(fd, bufsiz, flags)
```

- `MSG_WAITALL`
- `MSG_DONTROUTE`
- `MSG_PEEK`
- `MSG_OOB`

**Parameters:**

- `fd` (integer)
- `bufsiz` (integer?)
- `flags` (integer?): may have any combination (using bitwise OR) of:

**Returns:**

- `string`: data, integer ip, integer port

### rename

```lua
unix.rename(oldpath, newpath, olddirfd, newdirfd)
```

Renames file or directory.

**Parameters:**

- `oldpath` (string)
- `newpath` (string)
- `olddirfd` (integer)
- `newdirfd` (integer)

**Returns:**

- `true`

### rmdir

```lua
unix.rmdir(path, dirfd)
```

Removes empty directory at `path`.
Returns `ENOTDIR` if `path` isn't a directory, or a path component
in `path` exists yet wasn't a directory.

**Parameters:**

- `path` (string)
- `dirfd` (integer) *(optional)*

**Returns:**

- `true`

### rmrf

```lua
unix.rmrf(path)
```

Recursively removes filesystem path.
Like `unix.makedirs()` this function isn't actually a system call but
rather is a Libc convenience wrapper. It's intended to be equivalent
to using the UNIX shell's `rm -rf path` command.

**Parameters:**

- `path` (string): the file or directory path you wish to destroy.

**Returns:**

- `true`

### sched_yield

```lua
unix.sched_yield()
```

Relinquishes scheduled quantum.

### send

```lua
unix.send(fd, data, flags)
```

This is the same as `write` except it has a `flags` argument
that's intended for sockets.
- `MSG_NOSIGNAL`: Don't SIGPIPE on EOF
- `MSG_OOB`: Send stream data through out of bound channel
- `MSG_DONTROUTE`: Don't go through gateway (for diagnostics)
- `MSG_MORE`: Manual corking to belay nodelay (0 on non-Linux)

**Parameters:**

- `fd` (integer)
- `data` (string)
- `flags` (integer?): may have any combination (using bitwise OR) of:

**Returns:**

- `integer`: sent

### sendto

```lua
unix.sendto(fd, data, ip, port, flags)
```

This is useful for sending messages over UDP sockets to specific
addresses.
- `MSG_OOB`
- `MSG_DONTROUTE`
- `MSG_NOSIGNAL`

**Parameters:**

- `fd` (integer)
- `data` (string)
- `ip` (uint32)
- `port` (uint16)
- `flags` (integer) *(optional)*: may have any combination (using bitwise OR) of:

**Returns:**

- `integer`: sent

### setfsuid

```lua
unix.setfsuid(uid)
```

Sets user id for file system ops.

**Parameters:**

- `uid` (integer)

**Returns:**

- `true`

### setgid

```lua
unix.setgid(gid)
```

Sets group id.
Returns `ENOSYS` on Windows NT if `gid` isn't `getgid()`.

**Parameters:**

- `gid` (integer)

**Returns:**

- `true`

### setitimer

```lua
unix.setitimer(which, intervalsec, intervalns, valuesec, valuens)
```

Causes `SIGALRM` signals to be generated at some point(s) in the
future. The `which` parameter should be `ITIMER_REAL`.
Here's an example of how to create a 400 ms interval timer:
ticks = 0
assert(unix.sigaction(unix.SIGALRM, function(sig)
print('tick no. %d' % {ticks})
ticks = ticks + 1
end))
assert(unix.setitimer(unix.ITIMER_REAL, 0, 400e6, 0, 400e6))
while true do
unix.sigsuspend()
end
Here's how you'd do a single-shot timeout in 1 second:
unix.sigaction(unix.SIGALRM, MyOnSigAlrm, unix.SA_RESETHAND)
unix.setitimer(unix.ITIMER_REAL, 0, 0, 1, 0)

**Parameters:**

- `which` (integer)
- `intervalsec` (integer)
- `intervalns` (integer): needs to be on the interval `[0,1000000000)`
- `valuesec` (integer)
- `valuens` (integer): needs to be on the interval `[0,1000000000)`

**Returns:**

- `integer`: intervalsec, integer intervalns, integer valuesec, integer valuens

### setpgid

```lua
unix.setpgid(pid, pgid)
```

Sets process group id the modern way.

**Parameters:**

- `pid` (integer)
- `pgid` (integer)

**Returns:**

- `true`

### setpgrp

```lua
unix.setpgrp()
```

Sets process group id. This is the same as `setpgid(0,0)`.

**Returns:**

- `integer`: pgid

### setresgid

```lua
unix.setresgid(real, effective, saved)
```

Sets real, effective, and saved group ids.
If any of the above parameters are -1, then it's a no-op.
Returns `ENOSYS` on Windows NT.
Returns `ENOSYS` on Macintosh and NetBSD if `saved` isn't -1.

**Parameters:**

- `real` (integer)
- `effective` (integer)
- `saved` (integer)

**Returns:**

- `true`

### setresuid

```lua
unix.setresuid(real, effective, saved)
```

Sets real, effective, and saved user ids.
If any of the above parameters are -1, then it's a no-op.
Returns `ENOSYS` on Windows NT.
Returns `ENOSYS` on Macintosh and NetBSD if `saved` isn't -1.

**Parameters:**

- `real` (integer)
- `effective` (integer)
- `saved` (integer)

**Returns:**

- `true`

### setrlimit

```lua
unix.setrlimit(resource, soft, hard)
```

Changes resource limit.
- `RLIMIT_AS` limits the size of the virtual address space. This
will work on all platforms. It's emulated on XNU and Windows which
means it won't propagate across execve() currently.
- `RLIMIT_CPU` causes `SIGXCPU` to be sent to the process when the
soft limit on CPU time is exceeded, and the process is destroyed
when the hard limit is exceeded. It works everywhere but Windows
where it should be possible to poll getrusage() with setitimer().
- `RLIMIT_FSIZE` causes `SIGXFSZ` to sent to the process when the
soft limit on file size is exceeded and the process is destroyed
when the hard limit is exceeded. It works everywhere but Windows.
- `RLIMIT_NPROC` limits the number of simultaneous processes and it
should work on all platforms except Windows. Please be advised it
limits the process, with respect to the activities of the user id
as a whole.
- `RLIMIT_NOFILE` limits the number of open file descriptors and it
should work on all platforms except Windows (TODO).
If a limit isn't supported by the host platform, it'll be set to
127. On most platforms these limits are enforced by the kernel and
as such are inherited by subprocesses.

**Parameters:**

- `resource` (integer): may be one of:
- `soft` (integer)
- `hard` (integer) *(optional)*: defaults to whatever was specified in `soft`.

**Returns:**

- `true`

### setsid

```lua
unix.setsid()
```

Sets session id.
This function can be used to create daemons.
Fails with `ENOSYS` on Windows NT.

**Returns:**

- `integer`: sid

### setsockopt

```lua
unix.setsockopt(fd, level, optname, value)
```

Tunes networking parameters.
`level` and `optname` may be one of the following pairs. The ellipses
type signature above changes depending on which options are used.
`optname` is the option feature magic number. The constants for
these will be set to `0` if the option isn't supported on the host
platform.
Raises `ENOPROTOOPT` if your `level` / `optname` combination isn't
valid, recognized, or supported on the host platform.
Raises `ENOTSOCK` if `fd` is valid but isn't a socket.
Raises `EBADF` if `fd` isn't valid.
unix.getsockopt(fd:int, level:int, optname:int)
├─→ value:int
└─→ nil, unix.Errno
unix.setsockopt(fd:int, level:int, optname:int, value:bool)
├─→ true
└─→ nil, unix.Errno
- `SOL_SOCKET`, `SO_TYPE`
- `SOL_SOCKET`, `SO_DEBUG`
- `SOL_SOCKET`, `SO_ACCEPTCONN`
- `SOL_SOCKET`, `SO_BROADCAST`
- `SOL_SOCKET`, `SO_REUSEADDR`
- `SOL_SOCKET`, `SO_REUSEPORT`
- `SOL_SOCKET`, `SO_KEEPALIVE`
- `SOL_SOCKET`, `SO_DONTROUTE`
- `SOL_TCP`, `TCP_NODELAY`
- `SOL_TCP`, `TCP_CORK`
- `SOL_TCP`, `TCP_QUICKACK`
- `SOL_TCP`, `TCP_FASTOPEN_CONNECT`
- `SOL_TCP`, `TCP_DEFER_ACCEPT`
- `SOL_IP`, `IP_HDRINCL`
unix.getsockopt(fd:int, level:int, optname:int)
├─→ value:int
└─→ nil, unix.Errno
unix.setsockopt(fd:int, level:int, optname:int, value:int)
├─→ true
└─→ nil, unix.Errno
- `SOL_SOCKET`, `SO_SNDBUF`
- `SOL_SOCKET`, `SO_RCVBUF`
- `SOL_SOCKET`, `SO_RCVLOWAT`
- `SOL_SOCKET`, `SO_SNDLOWAT`
- `SOL_TCP`, `TCP_KEEPIDLE`
- `SOL_TCP`, `TCP_KEEPINTVL`
- `SOL_TCP`, `TCP_FASTOPEN`
- `SOL_TCP`, `TCP_KEEPCNT`
- `SOL_TCP`, `TCP_MAXSEG`
- `SOL_TCP`, `TCP_SYNCNT`
- `SOL_TCP`, `TCP_NOTSENT_LOWAT`
- `SOL_TCP`, `TCP_WINDOW_CLAMP`
- `SOL_IP`, `IP_TOS`
- `SOL_IP`, `IP_MTU`
- `SOL_IP`, `IP_TTL`
unix.getsockopt(fd:int, level:int, optname:int)
├─→ secs:int, nsecs:int
└─→ nil, unix.Errno
unix.setsockopt(fd:int, level:int, optname:int, secs:int[, nanos:int])
├─→ true
└─→ nil, unix.Errno
- `SOL_SOCKET`, `SO_RCVTIMEO`: If this option is specified then
your stream socket will have a read() / recv() timeout. If the
specified interval elapses without receiving data, then EAGAIN
shall be returned by read. If this option is used on listening
sockets, it'll be inherited by accepted sockets. Your redbean
already does this for GetClientFd() based on the `-t` flag.
- `SOL_SOCKET`, `SO_SNDTIMEO`: This is the same as `SO_RCVTIMEO`
but it applies to the write() / send() functions.
unix.getsockopt(fd:int, unix.SOL_SOCKET, unix.SO_LINGER)
├─→ seconds:int, enabled:bool
└─→ nil, unix.Errno
unix.setsockopt(fd:int, unix.SOL_SOCKET, unix.SO_LINGER, secs:int, enabled:bool)
├─→ true
└─→ nil, unix.Errno
This `SO_LINGER` parameter can be used to make close() a blocking
call. Normally when the kernel returns immediately when it receives
close(). Sometimes it's desirable to have extra assurance on errors
happened, even if it comes at the cost of performance.
unix.setsockopt(serverfd:int, unix.SOL_TCP, unix.TCP_SAVE_SYN, enabled:int)
├─→ true
└─→ nil, unix.Errno
unix.getsockopt(clientfd:int, unix.SOL_TCP, unix.TCP_SAVED_SYN)
├─→ syn_packet_bytes:str
└─→ nil, unix.Errno
This `TCP_SAVED_SYN` option may be used to retrieve the bytes of the
TCP SYN packet that the client sent when the connection for `fd` was
opened. In order for this to work, `TCP_SAVE_SYN` must have been set
earlier on the listening socket. This is Linux-only. You can use the
`OnServerListen` hook to enable SYN saving in your Redbean. When the
`TCP_SAVE_SYN` option isn't used, this may return empty string.

**Parameters:**

- `fd` (integer)
- `level` (integer)
- `optname` (integer)
- `value` (boolean|integer)

**Returns:**

- `true`

### setuid

```lua
unix.setuid(uid)
```

Sets user id.
One use case for this function is dropping root privileges. Should
you ever choose to run redbean as root and decide not to use the
`-G` and `-U` flags, you can replicate that behavior in the Lua
processes you spawn as follows:
ok, err = unix.setgid(1000)  -- check your /etc/groups
if not ok then Log(kLogFatal, tostring(err)) end
ok, err = unix.setuid(1000)  -- check your /etc/passwd
if not ok then Log(kLogFatal, tostring(err)) end
If your goal is to relinquish privileges because redbean is a setuid
binary, then things are more straightforward:
ok, err = unix.setgid(unix.getgid())
if not ok then Log(kLogFatal, tostring(err)) end
ok, err = unix.setuid(unix.getuid())
if not ok then Log(kLogFatal, tostring(err)) end
See also the setresuid() function and be sure to refer to your local
system manual about the subtleties of changing user id in a way that
isn't restorable.
Returns `ENOSYS` on Windows NT if `uid` isn't `getuid()`.

**Parameters:**

- `uid` (integer)

**Returns:**

- `true`

### shutdown

```lua
unix.shutdown(fd, how)
```

Partially closes socket.
- `SHUT_RD`: sends a tcp half close for reading
- `SHUT_WR`: sends a tcp half close for writing
- `SHUT_RDWR`
This system call currently has issues on Macintosh, so portable code
should log rather than assert failures reported by `shutdown()`.

**Parameters:**

- `fd` (integer)
- `how` (integer): is set to one of:

**Returns:**

- `true`

### sigaction

```lua
unix.sigaction(sig, handler, flags, mask)
```

- `unix.SIGINT`
- `unix.SIGQUIT`
- `unix.SIGTERM`
- etc.
- Lua function
- `unix.SIG_IGN`
- `unix.SIG_DFL`
- `unix.SA_RESTART`: Enables BSD signal handling semantics. Normally
i/o entrypoints check for pending signals to deliver. If one gets
delivered during an i/o call, the normal behavior is to cancel the
i/o operation and return -1 with `EINTR` in errno. If you use the
`SA_RESTART` flag then that behavior changes, so that any function
that's been annotated with @restartable will not return `EINTR`
and will instead resume the i/o operation. This makes coding
easier but it can be an anti-pattern if not used carefully, since
poor usage can easily result in latency issues. It also requires
one to do more work in signal handlers, so special care needs to
be given to which C library functions are @asyncsignalsafe.
- `unix.SA_RESETHAND`: Causes signal handler to be single-shot. This
means that, upon entry of delivery to a signal handler, it's reset
to the `SIG_DFL` handler automatically. You may use the alias
`SA_ONESHOT` for this flag, which means the same thing.
- `unix.SA_NODEFER`: Disables the reentrancy safety check on your signal
handler. Normally that's a good thing, since for instance if your
`SIGSEGV` signal handler happens to segfault, you're going to want
your process to just crash rather than looping endlessly. But in
some cases it's desirable to use `SA_NODEFER` instead, such as at
times when you wish to `longjmp()` out of your signal handler and
back into your program. This is only safe to do across platforms
for non-crashing signals such as `SIGCHLD` and `SIGINT`. Crash
handlers should use Xed instead to recover execution, because on
Windows a `SIGSEGV` or `SIGTRAP` crash handler might happen on a
separate stack and/or a separate thread. You may use the alias
`SA_NOMASK` for this flag, which means the same thing.
- `unix.SA_NOCLDWAIT`: Changes `SIGCHLD` so the zombie is gone and
you can't call wait() anymore; similar but may still deliver the
SIGCHLD.
- `unix.SA_NOCLDSTOP`: Lets you set `SIGCHLD` handler that's only
notified on exit/termination and not notified on `SIGSTOP`,
`SIGTSTP`, `SIGTTIN`, `SIGTTOU`, or `SIGCONT`.
Example:
function OnSigUsr1(sig)
gotsigusr1 = true
end
gotsigusr1 = false
oldmask = assert(unix.sigprocmask(unix.SIG_BLOCK, unix.Sigset(unix.SIGUSR1)))
assert(unix.sigaction(unix.SIGUSR1, OnSigUsr1))
assert(unix.raise(unix.SIGUSR1))
assert(not gotsigusr1)
ok, err = unix.sigsuspend(oldmask)
assert(not ok)
assert(err:errno() == unix.EINTR)
assert(gotsigusr1)
assert(unix.sigprocmask(unix.SIG_SETMASK, oldmask))
It's a good idea to not do too much work in a signal handler.

**Parameters:**

- `sig` (integer): can be one of:
- `handler` (function|integer) *(optional)*: can be:
- `flags` (integer) *(optional)*: can have:
- `mask` (unix.Sigset) *(optional)*

**Returns:**

- `function|integer`: oldhandler, integer flags, unix.Sigset mask

### sigprocmask

```lua
unix.sigprocmask(how, newmask)
```

Manipulates bitset of signals blocked by process.
- `SIG_BLOCK`: applies `mask` to set of blocked signals using bitwise OR
- `SIG_UNBLOCK`: removes bits in `mask` from set of blocked signals
- `SIG_SETMASK`: replaces process signal mask with `mask`
`mask` is a unix.Sigset() object (see section below).
For example, to temporarily block `SIGTERM` and `SIGINT` so critical
work won't be interrupted, sigprocmask() can be used as follows:
newmask = unix.Sigset(unix.SIGTERM)
oldmask = assert(unix.sigprocmask(unix.SIG_BLOCK, newmask))
-- do something...
assert(unix.sigprocmask(unix.SIG_SETMASK, oldmask))

**Parameters:**

- `how` (integer): can be one of:
- `newmask` (unix.Sigset)

**Returns:**

- `unix.Sigset`: oldmask

### sigsuspend

```lua
unix.sigsuspend(mask)
```

Waits for signal to be delivered.
The signal mask is temporarily replaced with `mask` during this system call.

**Parameters:**

- `mask` (unix.Sigset) *(optional)*: specifies which signals should be blocked.

**Returns:**

- `nil,`: unix.Errno error

### siocgifconf

```lua
unix.siocgifconf()
```

Returns list of network adapter addresses.

**Returns:**

- `{`: name: string, ip: integer, netmask: integer }[] addresses

### socket

```lua
unix.socket(family, type, protocol)
```

- `AF_INET`: Creates Internet Protocol Version 4 (IPv4) socket.
- `AF_UNIX`: Creates local UNIX domain socket. On the New Technology
this requires Windows 10 and only works with `SOCK_STREAM`.
- `SOCK_STREAM`
- `SOCK_DGRAM`
- `SOCK_RAW`
- `SOCK_RDM`
- `SOCK_SEQPACKET`
You may bitwise OR any of the following into `type`:
- `SOCK_CLOEXEC`
- `SOCK_NONBLOCK`
- `0` to let kernel choose [default]
- `IPPROTO_TCP`
- `IPPROTO_UDP`
- `IPPROTO_RAW`
- `IPPROTO_IP`
- `IPPROTO_ICMP`

**Parameters:**

- `family` (integer) *(optional)*: defaults to `AF_INET` and can be:
- `type` (integer) *(optional)*: defaults to `SOCK_STREAM` and can be:
- `protocol` (integer) *(optional)*: may be any of:

**Returns:**

- `integer`: fd

### socketpair

```lua
unix.socketpair(family, type, protocol)
```

Creates bidirectional pipe.
- `SOCK_STREAM`
- `SOCK_DGRAM`
- `SOCK_SEQPACKET`
You may bitwise OR any of the following into `type`:
- `SOCK_CLOEXEC`
- `SOCK_NONBLOCK`

**Parameters:**

- `family` (integer) *(optional)*: defaults to `AF_UNIX`.
- `type` (integer) *(optional)*: defaults to `SOCK_STREAM` and can be:
- `protocol` (integer) *(optional)*: defaults to `0`.

**Returns:**

- `integer`: fd1, integer fd2

### stat

```lua
unix.stat(path, flags, dirfd)
```

Gets information about file or directory.
- `AT_SYMLINK_NOFOLLOW`: do not follow symbolic links.

**Parameters:**

- `flags` (integer) *(optional)*: may have any of:
- `dirfd` (integer) *(optional)*: defaults to `unix.AT_FDCWD` and may optionally be set to a directory file descriptor to which `path` is relative.

**Returns:**

- `unix.Stat`

### strsignal

```lua
unix.strsignal(sig)
```

Turns platform-specific `sig` code into its symbolic name.
For example:
>: unix.strsignal(9)
"SIGKILL"
>: unix.strsignal(unix.SIGKILL)
"SIGKILL"
Please note that signal numbers are normally different across
supported platforms, and the constants should be preferred.

**Parameters:**

- `sig` (integer)

**Returns:**

- `string`: signalname

### symlink

```lua
unix.symlink(target, linkpath, newdirfd)
```

Creates symbolic link.
On Windows NT a symbolic link is called a "reparse point" and can
only be created from an administrator account. Your redbean will
automatically request the appropriate permissions.

**Parameters:**

- `target` (string)
- `linkpath` (string)
- `newdirfd` (integer) *(optional)*

**Returns:**

- `true`

### sync

```lua
unix.sync()
```

These functions are used to make programs slower by asking the
operating system to flush data to the physical medium.

### syslog

```lua
unix.syslog(priority, msg)
```

Generates a log message, which will be distributed by syslogd.
`priority` is a bitmask containing the facility value and the level
value. If no facility value is ORed into priority, then the default
value set by openlog() is used. If set to NULL, the program name is
used. Level is one of `LOG_EMERG`, `LOG_ALERT`, `LOG_CRIT`,
`LOG_ERR`, `LOG_WARNING`, `LOG_NOTICE`, `LOG_INFO`, `LOG_DEBUG`.
This function currently works on Linux, Windows, and NetBSD. On
WIN32 it uses the ReportEvent() facility.

**Parameters:**

- `priority` (integer)
- `msg` (string)

### tiocgwinsz

```lua
unix.tiocgwinsz(fd)
```

**Parameters:**

- `fd` (integer)

**Returns:**

- `integer`: rows, integer cols cellular dimensions of pseudoteletypewriter display.

### tmpfd

```lua
unix.tmpfd()
```

Returns file descriptor of open anonymous file.
This creates a secure temporary file inside `$TMPDIR`. If it isn't
defined, then `/tmp` is used on UNIX and GetTempPath() is used on
the New Technology. This resolution of `$TMPDIR` happens once.
Once close() is called, the returned file is guaranteed to be
deleted automatically. On UNIX the file is unlink()'d before this
function returns. On the New Technology it happens upon close().
On the New Technology, temporary files created by this function
should have better performance, because `kNtFileAttributeTemporary`
asks the kernel to more aggressively cache and reduce i/o ops.

**Returns:**

- `integer`: fd

### truncate

```lua
unix.truncate(path, length)
```

Reduces or extends underlying physical medium of file.
If file was originally larger, content >length is lost.

**Parameters:**

- `path` (string)
- `length` (integer) *(optional)*: defaults to zero (`0`)

**Returns:**

- `true`

### umask

```lua
unix.umask(newmask)
```

Sets file permission mask and returns the old one.
This is used to remove bits from the `mode` parameter of functions
like open() and mkdir(). The masks typically used are 027 and 022.
Those masks ensure that, even if a file is created with 0666 bits,
it'll be turned into 0640 or 0644 so that users other than the owner
can't modify it.
To read the mask without changing it, try doing this:
mask = unix.umask(027)
unix.umask(mask)
On Windows NT this is a no-op and `mask` is returned.
This function does not fail.

**Parameters:**

- `newmask` (integer)

**Returns:**

- `integer`: oldmask

### unlink

```lua
unix.unlink(path, dirfd)
```

Removes file at `path`.
If `path` refers to a symbolic link, the link is removed.
Returns `EISDIR` if `path` refers to a directory. See `rmdir()`.

**Parameters:**

- `path` (string)
- `dirfd` (integer) *(optional)*

**Returns:**

- `true`

### unveil

```lua
unix.unveil(path, permissions)
```

Restricts filesystem operations, e.g.
unix.unveil(".", "r");     -- current dir + children visible
unix.unveil("/etc", "r");  -- make /etc readable too
unix.unveil(nil, nil);     -- commit and lock policy
Unveiling restricts a thread's view of the filesystem to a set of
allowed paths with specific privileges.
Once you start using unveil(), the entire file system is considered
hidden. You then specify, by repeatedly calling unveil(), which paths
should become unhidden. When you're finished, you call `unveil(nil,nil)`
which commits your policy, after which further use is forbidden, in
the current thread, as well as any threads or processes it spawns.
There are some differences between unveil() on Linux versus OpenBSD.
1. Build your policy and lock it in one go. On OpenBSD, policies take
effect immediately and may evolve as you continue to call unveil()
but only in a more restrictive direction. On Linux, nothing will
happen until you call `unveil(nil,nil)` which commits and locks.
2. Try not to overlap directory trees. On OpenBSD, if directory trees
overlap, then the most restrictive policy will be used for a given
file. On Linux overlapping may result in a less restrictive policy
and possibly even undefined behavior.
3. OpenBSD and Linux disagree on error codes. On OpenBSD, accessing
paths outside of the allowed set raises ENOENT, and accessing ones
with incorrect permissions raises EACCES. On Linux, both these
cases raise EACCES.
4. Unlike OpenBSD, Linux does nothing to conceal the existence of
paths. Even with an unveil() policy in place, it's still possible
to access the metadata of all files using functions like stat()
and open(O_PATH), provided you know the path. A sandboxed process
can always, for example, determine how many bytes of data are in
/etc/passwd, even if the file isn't readable. But it's still not
possible to use opendir() and go fishing for paths which weren't
previously known.
This system call is supported natively on OpenBSD and polyfilled on
Linux using the Landlock LSM[1].
- `r` makes `path` available for read-only path operations,
corresponding to the pledge promise "rpath".
- `w` makes `path` available for write operations, corresponding
to the pledge promise "wpath".
- `x` makes `path` available for execute operations,
corresponding to the pledge promises "exec" and "execnative".
- `c` allows `path` to be created and removed, corresponding to
the pledge promise "cpath".

**Parameters:**

- `path` (string): is the file or directory to unveil
- `permissions` (string): is a string consisting of zero or more of the following characters:

**Returns:**

- `true`

### utimensat

```lua
unix.utimensat(path, asecs, ananos, msecs, mnanos, dirfd, flags)
```

Changes access and/or modified timestamps on file.
`path` is a string with the name of the file.
The `asecs` and `ananos` parameters set the access time. If they're
none or nil, the current time will be used.
The `msecs` and `mnanos` parameters set the modified time. If
they're none or nil, the current time will be used.
The nanosecond parameters (`ananos` and `mnanos`) must be on the
interval [0,1000000000) or `unix.EINVAL` is raised. On XNU this is
truncated to microsecond precision. On Windows NT, it's truncated to
hectonanosecond precision. These nanosecond parameters may also be
set to one of the following special values:
- `unix.UTIME_NOW`: Fill this timestamp with current time. This
feature is not available on old versions of Linux, e.g. RHEL5.
- `unix.UTIME_OMIT`: Do not alter this timestamp. This feature is
not available on old versions of Linux, e.g. RHEL5.
`dirfd` is a file descriptor integer opened with `O_DIRECTORY`
that's used for relative path names. It defaults to `unix.AT_FDCWD`.
`flags` may have have any of the following flags bitwise or'd
- `AT_SYMLINK_NOFOLLOW`: Do not follow symbolic links. This makes it
possible to edit the timestamps on the symbolic link itself,
rather than the file it points to.

**Parameters:**

- `path` (string)
- `asecs` (integer)
- `ananos` (integer)
- `msecs` (integer)
- `mnanos` (integer)
- `dirfd` (integer) *(optional)*
- `flags` (integer) *(optional)*

**Returns:**

- `0`

### wait

```lua
unix.wait(pid, options)
```

Waits for subprocess to terminate.
`pid` defaults to `-1` which means any child process. Setting
`pid` to `0` is equivalent to `-getpid()`. If `pid < -1` then
that means wait for any pid in the process group `-pid`. Then
lastly if `pid > 0` then this waits for a specific process id
Options may have `WNOHANG` which means don't block, check for
the existence of processes that are already dead (technically
speaking zombies) and if so harvest them immediately.
Returns the process id of the child that terminated. In other
cases, the returned `pid` is nil and `errno` is non-nil.
The returned `wstatus` contains information about the process
exit status. It's a complicated integer and there's functions
that can help interpret it. For example:
-- wait for zombies
-- traditional technique for SIGCHLD handlers
while true do
pid, status = unix.wait(-1, unix.WNOHANG)
if pid then
if unix.WIFEXITED(status) then
print('child', pid, 'exited with',
unix.WEXITSTATUS(status))
elseif unix.WIFSIGNALED(status) then
print('child', pid, 'crashed with',
unix.strsignal(unix.WTERMSIG(status)))
end
elseif status:errno() == unix.ECHILD then
Log(kLogDebug, 'no more zombies')
break
else
Log(kLogWarn, tostring(err))
break
end
end

**Parameters:**

- `pid` (integer) *(optional)*
- `options` (integer) *(optional)*

**Returns:**

- `integer`: pid, integer wstatus, unix.Rusage rusage

### write

```lua
unix.write(fd, data, offset)
```

Writes to file descriptor.

**Parameters:**

- `fd` (integer)
- `data` (string)
- `offset` (integer?)

**Returns:**

- `integer`: wrotebytes
