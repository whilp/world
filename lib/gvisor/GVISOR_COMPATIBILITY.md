# gVisor compatibility investigation

## Summary

The assimilated binaries in `assimilated-linux-x86_64.tar.gz` **do not work in gVisor environments** (including claude.ai/code) due to fundamental incompatibility with gVisor's syscall implementation.

## Root cause

All assimilated binaries (lua, make, curl, zip, unzip) crash with segmentation fault when attempting to set up thread-local storage:

```
arch_prctl(ARCH_SET_GS, 0x66ae00) = -1 EINVAL (Invalid argument)
--- SIGSEGV {si_signo=SIGSEGV, si_code=SEGV_MAPERR, si_addr=0x6fe000004} ---
```

### Why this happens

1. **Cosmopolitan's assimilation process**: The `assimilate` tool converts APE binaries to native ELF binaries that use the `%gs` register for thread-local storage

2. **gVisor's limitation**: gVisor explicitly does NOT support `ARCH_SET_GS` and `ARCH_GET_GS` operations in its `arch_prctl` syscall implementation

3. **Incompatibility**: When assimilated binaries try to set up TLS using `ARCH_SET_GS`, gVisor returns EINVAL, causing segmentation fault

## Environment status

### Works in gVisor

These tools are already available via system packages in gVisor environments:

- **make** - GNU Make 4.3
- **curl** - curl 8.5.0 with TLS support
- **zip** - Info-ZIP
- **unzip** - Info-ZIP

### Does not work in gVisor

- **lua** - assimilated binary crashes
- **lua with cosmo modules** - not available

## Alternative approaches

### 1. Use system packages (recommended)

For make, curl, zip, unzip - use system versions directly.

For lua:
```bash
sudo apt-get install lua5.4
```

Note: System lua does not include cosmo.* modules.

### 2. Non-assimilated APE binaries

Original APE binaries might work if they use `%fs` register instead of `%gs`. Needs testing.

### 3. Standard dynamic linking

Build lua as standard ELF binary instead of using Cosmopolitan/APE:
- Works in gVisor
- Loses portability
- Requires platform-specific builds
- Cosmo modules need separate compilation

## Recommendations

### Short term

1. Update skill documentation to clarify assimilated binaries work in standard Linux but NOT gVisor
2. Rename "gvisor-tools" skill to avoid confusion
3. Document workarounds for gVisor environments

### Long term

1. Build non-assimilated versions using `%fs` register
2. Provide platform-specific builds for gVisor
3. Create separate gVisor-compatible package without assimilation

## Technical references

- [gVisor syscall compatibility](https://gvisor.dev/docs/user_guide/compatibility/)
- [gVisor arch implementation](https://github.com/google/gvisor/blob/master/pkg/sentry/arch/arch_x86.go)
- [Cosmopolitan assimilate tool](https://github.com/jart/cosmopolitan/blob/master/tool/build/assimilate.c)
- [arch_prctl syscall](https://man7.org/linux/man-pages/man2/arch_prctl.2.html)

## Testing evidence

```bash
# Assimilated binaries fail in gVisor:
$ ~/gvisor/bin/lua -v
Segmentation fault (exit code 139)

$ strace ~/gvisor/bin/lua -v 2>&1 | grep arch_prctl
arch_prctl(ARCH_SET_GS, 0x66ae00) = -1 EINVAL (Invalid argument)

# System tools work:
$ make --version
GNU Make 4.3

$ curl --version
curl 8.5.0 (x86_64-pc-linux-gnu)
```

## Conclusion

Assimilated binaries are incompatible with gVisor due to syscall differences. They work in standard Linux (verified by GitHub Actions) but cannot be used in gVisor sandboxes like claude.ai/code.
