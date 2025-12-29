# cosmo.re

### BADBR

```lua
re.BADBR (constant)
```

Invalid contents of {}

### BADPAT

```lua
re.BADPAT (constant)
```

Invalid regex

### BADRPT

```lua
re.BADRPT (constant)
```

Repetition not preceded by valid expression

### BASIC

```lua
re.BASIC (constant)
```

Use this flag if you prefer the default POSIX regex syntax.
We use extended regex notation by default. For example, an extended regular
expression for matching an IP address might look like
`([0-9]*)\.([0-9]*)\.([0-9]*)\.([0-9]*)` whereas with basic syntax it would
look like `\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\)`.
This flag may only be used with `re.compile` and `re.search`.

### EBRACE

```lua
re.EBRACE (constant)
```

Missing }

### EBRACK

```lua
re.EBRACK (constant)
```

Missing ]

### ECOLLATE

```lua
re.ECOLLATE (constant)
```

Unknown collating element

### ECTYPE

```lua
re.ECTYPE (constant)
```

Unknown character class name

### EESCAPE

```lua
re.EESCAPE (constant)
```

Trailing backslash

### EPAREN

```lua
re.EPAREN (constant)
```

Missing )

### ERANGE

```lua
re.ERANGE (constant)
```

Invalid character range.

### ESPACE

```lua
re.ESPACE (constant)
```

Out of memory

### ESUBREG

```lua
re.ESUBREG (constant)
```

Invalid back reference

### ICASE

```lua
re.ICASE (constant)
```

Use this flag if you prefer the default POSIX regex syntax. We use extended
regex notation by default. For example, an extended regular expression for
matching an IP address might look like `([0-9]*)\.([0-9]*)\.([0-9]*)\.([0-9]*)`
whereas with basic syntax it would look like `\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\)`.
This flag may only be used with `re.compile` and `re.search`.

### NEWLINE

```lua
re.NEWLINE (constant)
```

Use this flag to change the handling of NEWLINE (\x0a) characters. When this
flag is set, (1) a NEWLINE shall not be matched by a "." or any form of a
non-matching list, (2) a "^" shall match the zero-length string immediately
after a NEWLINE (regardless of `re.NOTBOL`), and (3) a "$" shall match the
zero-length string immediately before a NEWLINE (regardless of `re.NOTEOL`).

### NOMATCH

```lua
re.NOMATCH (constant)
```

This module exposes an API for POSIX regular expressions which enable you to
validate input, search for substrings, extract pieces of strings, etc.
Here's a usage example:
-- Example IPv4 Address Regular Expression (see also ParseIP)
p = assert(re.compile([[^([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})---  $]]))
m,a,b,c,d = assert(p:search(𝑠))
if m then
print("ok", tonumber(a), tonumber(b), tonumber(c), tonumber(d))
else
print("not ok")
end
No match

### NOSUB

```lua
re.NOSUB (constant)
```

Causes `re.search` to only report success and failure. This is reported via
the API by returning empty string for success. This flag may only be used
` with `re.compile` and `re.search`.

### NOTBOL

```lua
re.NOTBOL (constant)
```

The first character of the string pointed to by string is not the beginning
of the line. This flag may only be used with `re.search` and `regex_t*:search`.

### NOTEOL

```lua
re.NOTEOL (constant)
```

The last character of the string pointed to by string is not the end of the
line. This flag may only be used with `re.search` and `regex_t*:search`.

### Regex

```lua
re.Regex
```

- `unix.NOMATCH` No match
- `unix.BADPAT` Invalid regex
- `unix.ECOLLATE` Unknown collating element
- `unix.ECTYPE` Unknown character class name
- `unix.EESCAPE` Trailing backslash
- `unix.ESUBREG` Invalid back reference
- `unix.EBRACK` Missing `]`
- `unix.EPAREN` Missing `)`
- `unix.EBRACE` Missing `}`
- `unix.BADBR` Invalid contents of `{}`
- `unix.ERANGE` Invalid character range.
- `unix.ESPACE` Out of memory
- `unix.BADRPT` Repetition not preceded by valid expression
Delegates to `re.Errno:doc()`.

### compile

```lua
re.compile(regex, flags)
```

Compiles regular expression.
- `re.BASIC`
- `re.ICASE`
- `re.NEWLINE`
- `re.NOSUB`
This has an O(2^𝑛) cost. Consider compiling regular expressions once
from your `/.init.lua` file.
If regex is an untrusted user value, then `unix.setrlimit` should be
used to impose cpu and memory quotas for security.
This uses POSIX extended syntax by default.

**Parameters:**

- `regex` (string)
- `flags` (integer?): defaults to zero and may have any of:

**Returns:**

- `re.Regex`

### search

```lua
re.search(regex, text, flags)
```

Executes precompiled regular expression.
Returns nothing (`nil`) if the pattern doesn't match anything. Otherwise it
pushes the matched substring and any parenthesis-captured values too. Flags may
contain `re.NOTBOL` or `re.NOTEOL` to indicate whether or not text should be
considered at the start and/or end of a line.
- `re.NOTBOL`
- `re.NOTEOL`
This has an O(𝑛) cost.
Searches for regular expression match in text.
This is a shorthand notation roughly equivalent to:
preg = re.compile(regex)
patt = preg:search(re, text)
- `re.BASIC`
- `re.ICASE`
- `re.NEWLINE`
- `re.NOSUB`
- `re.NOTBOL`
- `re.NOTEOL`
This has exponential complexity. Please use `re.compile()` to compile your regular expressions once from `/.init.lua`. This API exists for convenience. This isn't recommended for prod.
This uses POSIX extended syntax by default.

**Parameters:**

- `str` (string)
- `flags` (integer) *(optional)*: defaults to zero and may have any of:
- `regex` (string)
- `text` (string)
- `flags` (integer?): defaults to zero and may have any of:

**Returns:**

- `string`: match, string ... the match, followed by any captured groups
- `string`: match, string ... the match, followed by any captured groups
