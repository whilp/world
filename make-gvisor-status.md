# make binary gVisor compatibility status

## Current status

❌ **Does not work on gVisor**

## Binary details

**Location**: `/home/user/dotfiles/3p/make/make`
**SHA256**: `4b046a78e1903add255bb590af66c5e7dc4c3690c8a3b351037d4a3609e5e9c1`
**Size**: 903,168 bytes (883 KB)
**Format**: APE (Actually Portable Executable)
**Type**: DOS/MBR boot sector (Cosmopolitan APE)

## Test results

```bash
$ /home/user/dotfiles/3p/make/make --version
Segmentation fault (exit 139)

$ bash /home/user/dotfiles/3p/make/make --version
Segmentation fault (exit 139)

$ sh /home/user/dotfiles/3p/make/make --version
Segmentation fault (exit 139)
```

## Root cause

Same issue as `lua-native-test`:
- Built with older Cosmopolitan Libc
- Uses `ARCH_SET_GS` only (no fallback to `ARCH_SET_FS`)
- gVisor rejects `ARCH_SET_GS` with EINVAL
- Binary crashes immediately

## Comparison with working lua binaries

| Binary | Works on gVisor | TLS Strategy |
|--------|----------------|--------------|
| make (current) | ❌ | GS only |
| lua-native-test | ❌ | GS only |
| lua-gvisor-test | ✅ | FS only |
| lua-unified-x86_64| ✅ | GS → FS fallback |

## Solution

The make binary needs to be rebuilt with:
- Newer Cosmopolitan Libc version with gVisor support
- Smart fallback mechanism (try GS, fall back to FS)
- Same approach as `lua-unified-x86_64`

## Recommendation

**Action needed**: Rebuild make binary with gVisor-compatible Cosmopolitan build

This would require the same build process that was used for `lua-unified-x86_64`, ensuring the binary can detect and adapt to the runtime environment.
