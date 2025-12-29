# cosmo.path

### basename

```lua
path.basename(str)
```

Returns final component of path, e.g.
path      │ basename
─────────────────────
.         │ .
..        │ ..
/         │ /
usr       │ usr
/usr/     │ usr
/usr/lib  │ lib
/usr/lib/ │ lib

**Parameters:**

- `str` (string)

**Returns:**

- `string`

### dirname

```lua
path.dirname(str)
```

The path module may be used to manipulate unix paths.
Note that we use unix paths on Windows. For example, if you have a
path like `C:\foo\bar` then it should be `/c/foo/bar` with redbean.
It should also be noted the unix module is more permissive when
using Windows paths, where translation to win32 is very light.
Strips final component of path, e.g.
path      │ dirname
───────────────────
.         │ .
..        │ .
/         │ /
usr       │ .
/usr/     │ /
/usr/lib  │ /usr
/usr/lib/ │ /usr

**Parameters:**

- `str` (string)

**Returns:**

- `string`

### exists

```lua
path.exists(path)
```

Returns `true` if path exists.
This function is inclusive of regular files, directories, and special files.
Symbolic links are followed are resolved. On error, `false` is returned.

**Parameters:**

- `path` (string)

**Returns:**

- `boolean`

### isdir

```lua
path.isdir(path)
```

Returns `true` if path exists and is directory.
Symbolic links are not followed. On error, `false` is returned.

**Parameters:**

- `path` (string)

**Returns:**

- `boolean`

### isfile

```lua
path.isfile(path)
```

Returns `true` if path exists and is regular file.
Symbolic links are not followed. On error, `false` is returned.

**Parameters:**

- `path` (string)

**Returns:**

- `boolean`

### islink

```lua
path.islink(path)
```

Returns `true` if path exists and is symbolic link.
Symbolic links are not followed. On error, `false` is returned.

**Parameters:**

- `path` (string)

**Returns:**

- `boolean`

### join

```lua
path.join(str, ...)
```

Concatenates path components, e.g.
x         │ y        │ joined
─────────────────────────────────
/         │ /        │ /
/usr      │ lib      │ /usr/lib
/usr/     │ lib      │ /usr/lib
/usr/lib  │ /lib     │ /lib
You may specify 1+ arguments.
Specifying no arguments will raise an error. If `nil` arguments are specified,
then they're skipped over. If exclusively `nil` arguments are passed, then `nil`
is returned. Empty strings behave similarly to `nil`, but unlike `nil` may
coerce a trailing slash.

**Parameters:**

- `str` (string?)

**Returns:**

- `string?`
