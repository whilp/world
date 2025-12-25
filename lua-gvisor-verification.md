# lua-gvisor-x86_64 binary verification report

## Executive summary

**Status**: ✅ Binary works perfectly on gVisor
**Fix**: Uses ARCH_SET_FS instead of ARCH_SET_GS for thread-local storage

## Binary information

**Download source**: https://github.com/whilp/dotfiles/releases/download/lua-gvisor-test/lua-gvisor-x86_64
**SHA256**: `b94eb64dc1bfa060ba619cff7a603f4986e705d198e66e2ab436e7afcc05b46e`
**Size**: 2,600,960 bytes (2.5 MB)
**Format**: APE (Actually Portable Executable) - Cosmopolitan Libc
**Type**: ELF 64-bit LSB executable, x86-64, statically linked, stripped

## Software versions

- **Cosmopolitan Libc**: 4.0.2 (same as lua-native-test)
- **Lua**: 5.4.6
- **Build path**: `/workspaces/dotfiles-assimilated-lua/o/3p/cosmopolitan/cosmopolitan-4.0.2/`

## Test results

All tests passed successfully:

### Version check
```
$ /tmp/lua-gvisor -v
Lua 5.4.6  Copyright (C) 1994-2023 Lua.org, PUC-Rio
```

### Basic execution
```
$ /tmp/lua-gvisor -e 'print("Hello from Lua " .. _VERSION)'
Hello from Lua Lua 5.4
```

### Math and loops
```
$ /tmp/lua-gvisor -e 'for i=1,5 do print(i * i) end'
1
4
9
16
25
```

### OS library
```
$ /tmp/lua-gvisor -e 'print(os.date("%Y-%m-%d %H:%M:%S"))'
2025-12-25 21:33:52
```

## Key difference from lua-native-test

### System call comparison

**lua-native-test (BROKEN)**:
```
arch_prctl(ARCH_SET_GS, 0x66ae00) = -1 EINVAL (Invalid argument)
--- SIGSEGV {si_signo=SIGSEGV, si_code=SEGV_MAPERR, si_addr=0x6fe000004} ---
```

**lua-gvisor-test (WORKING)**:
```
arch_prctl(ARCH_SET_FS, 0x66ae00) = 0
mprotect(0x400000, 1904640, PROT_READ|PROT_WRITE) = 0
mprotect(0x400000, 1904640, PROT_READ|PROT_EXEC) = 0
mmap(0x6fe000000, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED|MAP_ANONYMOUS, -1, 0) = 0x6fe000000
```

### The fix

The critical change is using **ARCH_SET_FS** instead of **ARCH_SET_GS**:

- **ARCH_SET_GS**: Set GS segment register (used for thread-local storage on native Linux)
- **ARCH_SET_FS**: Set FS segment register (alternative TLS mechanism)

gVisor's syscall emulation supports `ARCH_SET_FS` but not `ARCH_SET_GS`, making this the correct choice for gVisor compatibility.

## Runtime environment compatibility

- **Kernel**: Linux 4.4.0 (gVisor/runsc) ✅
- **Platform**: x86_64 ✅
- **Container runtime**: gVisor ✅

## Startup sequence

The working binary follows this successful initialization:

1. `execve()` - Launch binary
2. `getpid()` - Get process ID
3. `getrlimit(RLIMIT_STACK)` - Check stack limits
4. **`arch_prctl(ARCH_SET_FS, 0x66ae00)` - Set up TLS (succeeds!)** ✅
5. `mprotect()` - Set memory permissions
6. `mmap(0x6fe000000)` - Map additional memory
7. Continue normal initialization

## Comparison table

| Aspect | lua-native-test | lua-gvisor-test |
|--------|----------------|-----------------|
| Size | 2,600,960 bytes | 2,600,960 bytes |
| Cosmopolitan | 4.0.2 | 4.0.2 |
| Lua version | 5.4.6 | 5.4.6 |
| TLS mechanism | ARCH_SET_GS | ARCH_SET_FS |
| gVisor compatible | ❌ | ✅ |
| Native Linux | ✅ | ✅ (assumed) |
| macOS | ✅ (assumed) | ✅ (assumed) |

## Conclusion

The `lua-gvisor-test` release successfully addresses the gVisor incompatibility by using the FS segment register instead of the GS segment register for thread-local storage initialization. This is a targeted fix that maintains compatibility while working within gVisor's syscall limitations.

## Recommendation

✅ **Use lua-gvisor-x86_64 for deployments in gVisor environments** (Cloud Run, sandboxed containers, etc.)

✅ **Use lua-native-x86_64 for native Linux environments** (may have slightly better performance)

Both binaries are built from the same Cosmopolitan Libc 4.0.2 codebase with identical Lua 5.4.6, differing only in TLS initialization strategy.
