# cosmo.re

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
