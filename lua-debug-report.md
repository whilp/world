# lua-linux-x86_64 binary debug report

## Executive summary

**Status**: Binary crashes immediately with segmentation fault
**Root cause**: gVisor kernel emulation incompatibility with Cosmopolitan Libc's thread-local storage initialization

## Binary information

**Download source**: https://github.com/whilp/dotfiles/releases/download/lua-native-test/lua-linux-x86_64
**SHA256**: `4d2d7c9ef4a6a46bed45e7f1decd70ba2c0b90bab274d3444dcbd9edcb55dcd7`
**Size**: 2,600,960 bytes (2.5 MB)
**Format**: APE (Actually Portable Executable) - Cosmopolitan Libc
**Type**: ELF 64-bit LSB executable, x86-64, statically linked, stripped

## Software versions

- **Cosmopolitan Libc**: 4.0.2
- **Lua**: 5.4.6
- **Compiler**: GNU C23 14.1.0
- **Build path**: `/workspaces/dotfiles-assimilated-lua/o/3p/cosmopolitan/cosmopolitan-4.0.2/`

## Runtime environment

- **Kernel**: Linux 4.4.0 (gVisor/runsc)
- **Platform**: x86_64
- **Container runtime**: gVisor (not native Linux)
- **Architecture**: 64-bit (LONG_BIT=64, WORD_BIT=32)

## Crash analysis

### System call trace

```
execve("/tmp/lua-test", ["/tmp/lua-test", "-v"], [...]) = 0
getpid()                                = <pid>
getrlimit(RLIMIT_STACK, {rlim_cur=8192*1024, rlim_max=RLIM64_INFINITY}) = 0
arch_prctl(ARCH_SET_GS, 0x66ae00)       = -1 EINVAL (Invalid argument)
--- SIGSEGV {si_signo=SIGSEGV, si_code=SEGV_MAPERR, si_addr=0x6fe000004} ---
+++ killed by SIGSEGV +++
```

### Failure sequence

1. Binary executes successfully
2. Attempts to set up thread-local storage (TLS) via `arch_prctl(ARCH_SET_GS, 0x66ae00)`
3. gVisor returns `EINVAL` (Invalid argument) - rejecting the GS register setup
4. Binary attempts to use TLS anyway, dereferencing invalid address
5. Immediate segmentation fault at address `0x6fe000004`

### TLS address analysis

- **GS register target**: `0x66ae00` (6,729,216 decimal)
- **Fault address**: `0x6fe000004` (30,031,216,644 decimal)
- **TLS section (.tbss)**: Address `0x61b000`, size `0x40` bytes
- **Data section (.data)**: Address `0x61b000`, size `0x4e000` bytes
- **BSS section (.bss)**: Address `0x669000`, size `0x3d000` bytes

The GS target address (0x66ae00) falls within the BSS section range, which is reasonable for TLS setup.

## Build configuration

### Key compiler flags

All code compiled with these common flags:
- `-mno-tls-direct-seg-refs` - Avoid direct TLS segment references
- `-mno-red-zone` - Disable red zone optimization
- `-mno-omit-leaf-frame-pointer` - Keep frame pointers
- `-march=x86-64 -mtune=generic` - Generic x86-64 target
- `-g -ggdb -O2` - Debug symbols with optimization
- `-std=gnu23` - GNU C23 standard
- `-fno-gnu-unique` - Disable GNU unique symbols
- `-fstrict-aliasing -fstrict-overflow`
- `-fno-semantic-interposition`

### Embedded resources

Binary contains a `.zip` section (39,548 bytes) with:
- Timezone data (`usr/share/zoneinfo/*`)
- Lua standard library scripts (`/zip/.lua/?.lua`)
- Default Lua package path: `/zip/.lua/?.lua;/zip/.lua/?/init.lua;./?.lua;./?/init.lua`

## ELF structure

### Program headers

```
Type           Offset     VirtAddr   PhysAddr   FileSiz    MemSiz     Flags  Align
LOAD           0x00004000 0x00400000 0x00002000 0x00001000 0x00001000 R E    0x4000  (.head)
LOAD           0x00005000 0x00401000 0x00003000 0x001da000 0x001da000 R E    0x4000  (.text)
LOAD           0x001df000 0x005db000 0x001dd000 0x0003f000 0x0003f000 R      0x4000  (.rodata)
TLS            0x0021f000 0x0061b000 0x0021d000 0x00000000 0x00000040 RW     0x40    (.tbss)
LOAD           0x0021f000 0x0061b000 0x0021d000 0x0004e000 0x0008b000 RW     0x4000  (.data/.bss)
GNU_STACK      0x00000000 0x00000000 0x00000000 0x00000000 0x00000000 RW     0x8
```

### Key sections

- `.head`: 4,096 bytes - APE bootstrap code
- `.text`: 1,941,504 bytes - Executable code
- `.rodata`: 258,048 bytes - Read-only data
- `.data`: 319,488 bytes - Initialized data
- `.bss`: 249,856 bytes - Uninitialized data
- `.tbss`: 64 bytes - Thread-local storage
- `.zip`: 39,548 bytes - Embedded ZIP archive
- `.GCC.command.line`: 17,140 bytes - Build metadata

### Entry point

Address: `0x401577`
First instructions initialize stack and data sections before calling main.

## APE (Actually Portable Executable) features

The binary starts with shell script wrapper:

```bash
m=$(uname -m 2>/dev/null) || m=x86_64
if [ "$m" = x86_64 ] || [ "$m" = amd64 ]; then
  [ x"$1" != x--assimilate ] && type ape >/dev/null 2>&1 && exec ape "$o" "$@"
  # ... rewrites ELF header on-the-fly for compatibility ...
fi
```

This allows the same binary to run on:
- Linux x86-64
- macOS x86-64
- Windows x86-64 (with WSL or APE runtime)
- OpenBSD, FreeBSD, NetBSD

## Root cause analysis

### Why it fails

1. **Cosmopolitan Libc** requires `arch_prctl(ARCH_SET_GS)` to initialize thread-local storage
2. **gVisor** (Linux 4.4.0 emulation) doesn't properly support this syscall
3. The syscall returns `EINVAL` instead of succeeding
4. Binary continues execution assuming TLS is set up
5. First TLS access triggers segmentation fault

### gVisor limitation

gVisor implements a subset of Linux system calls in user space for security sandboxing. The `arch_prctl` implementation in gVisor 4.4.0 doesn't fully support the GS register setup that Cosmopolitan requires.

## Attempted workarounds

None of these helped:
- `COSMOPOLITAN_DISABLE_ZIPOS=1` - Still crashes
- Running without arguments - Still crashes
- All invocation modes result in identical failure

## Compatibility assessment

**Works on**: Real Linux kernel 4.4+, macOS, Windows (with APE runtime)
**Fails on**: gVisor sandboxed environments

## Recommendations

1. **For testing**: Use native Linux kernel, not gVisor
2. **For CI/CD**: Use Docker with native runc, not gVisor
3. **For cloud**: Ensure runtime doesn't use gVisor (e.g., avoid some GCP Cloud Run configurations)
4. **Alternative**: Build non-Cosmopolitan static Lua binary for gVisor environments

## Additional information

### Copyright and licenses

- Cosmopolitan Libc: Copyright 2024 Justine Alexandra Roberts Tunney
- Lua: Copyright (C) 1994-2023 Lua.org, PUC-Rio
- Largon2: MIT License, Copyright 2016 Thibault Charbonnier
- Linenoise: BSD-2, Copyright 2018-2020 Justine Tunney

### Build metadata

Full GCC command lines preserved in `.GCC.command.line` section show extensive compilation with multiple optimization levels (-O2, -O3, -Os) for different components.

### References

- Cosmopolitan Libc: https://justine.lol/cosmopolitan/
- APE format: https://justine.lol/ape.html
- gVisor: https://gvisor.dev/
