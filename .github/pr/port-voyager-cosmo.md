# voyager: port fetch script to cosmo

Port the voyager keyboard layout fetch script from luaposix to cosmo.

## Changes

- `.config/voyager/fetch` - rewrote to use cosmo APIs

## API replacements

| Before (posix) | After (cosmo) |
|----------------|---------------|
| `posix.getenv("HOME")` | `os.getenv("HOME")` |
| `posix.mkdtemp()` | `unix.mkdtemp()` |
| `posix.mkdir()` | `unix.makedirs()` |
| `os.execute("curl ...")` | `cosmo.Fetch()` + `cosmo.Barf()` |
| `os.execute("unzip ...")` | `spawn({...}):wait()` |
| `os.execute("mv ...")` | `unix.opendir()` / `unix.rename()` |
| `os.execute("rm -rf ...")` | `unix.rmrf()` |
| String concatenation for paths | `path.join()` |

## Validation

- [x] Script passes syntax check
