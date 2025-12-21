# cosmo.re

POSIX regular expression engine for Lua, provided by the `cosmo` module.

## Usage

```lua
local cosmo = require("cosmo")
local re = cosmo.re
```

## Functions

### compile

```lua
regex, err = re.compile(pattern [, flags])
```

Compile regular expression pattern. Returns regex object on success, or `nil, err` on failure.

Flags can be bitwise OR of:
- `re.BASIC` - use basic regular expressions
- `re.ICASE` - case insensitive matching
- `re.NEWLINE` - newline-sensitive matching
- `re.NOSUB` - don't report subexpression matches

```lua
local regex, err = re.compile('[0-9]+')
if not regex then
  print("Compile error: " .. err)
end
```

### search

```lua
match = re.search(pattern, text [, flags])
```

Search for pattern in text. Returns first match as string, or `nil` if no match.

```lua
local match = re.search('[0-9]+', 'hello 123 world')
if match then
  print("Found: " .. match)  -- prints "Found: 123"
end
```

Case-insensitive search:
```lua
local match = re.search('HELLO', 'hello world', re.ICASE)
-- returns "hello"
```

## Flags

### BASIC

Use basic regular expressions instead of extended.

### ICASE

Perform case-insensitive matching.

```lua
local match = re.search('FOO', 'foobar', re.ICASE)
-- matches "foo"
```

### NEWLINE

Newline-sensitive matching:
- `.` does not match newline
- `^` matches start of line (not just start of string)
- `$` matches end of line (not just end of string)

### NOSUB

Do not report subexpression matches (faster).

### NOTBOL

Do not treat beginning of text as beginning of line (for `^` anchor).

### NOTEOL

Do not treat end of text as end of line (for `$` anchor).

## Error codes

Error codes returned by `compile`:

- `BADBR` - invalid content in `\{\}`
- `BADPAT` - invalid regular expression
- `BADRPT` - invalid use of repetition operators
- `EBRACE` - unmatched `\{` or `\}`
- `EBRACK` - unmatched `[` or `]`
- `ECOLLATE` - invalid collating element
- `ECTYPE` - invalid character class name
- `EESCAPE` - trailing backslash
- `EPAREN` - unmatched `(` or `)`
- `ERANGE` - invalid range in `[]`
- `ESPACE` - out of memory
- `ESUBREG` - invalid back reference
- `NOMATCH` - pattern did not match

## Pattern syntax

POSIX extended regular expressions (default):

**Anchors:**
- `^` - start of line/string
- `$` - end of line/string

**Character classes:**
- `.` - any character
- `[abc]` - any of a, b, or c
- `[^abc]` - not a, b, or c
- `[a-z]` - range from a to z
- `[:alnum:]` - alphanumeric characters
- `[:alpha:]` - alphabetic characters
- `[:digit:]` - digits
- `[:lower:]` - lowercase letters
- `[:upper:]` - uppercase letters
- `[:space:]` - whitespace
- `[:punct:]` - punctuation

**Quantifiers:**
- `*` - zero or more
- `+` - one or more
- `?` - zero or one
- `{n}` - exactly n times
- `{n,}` - n or more times
- `{n,m}` - between n and m times

**Groups:**
- `(pattern)` - capturing group
- `|` - alternation

**Escapes:**
- `\` - escape special character

## Examples

### Basic pattern matching

```lua
local re = cosmo.re

-- Match digits
local num = re.search('[0-9]+', 'Price: $42.50')
print(num)  -- prints "42"

-- Match word
local word = re.search('[a-zA-Z]+', '123 hello 456')
print(word)  -- prints "hello"

-- Match email-like pattern
local email = re.search('[a-z]+@[a-z]+\\.com', 'contact user@example.com here')
print(email)  -- prints "user@example.com"
```

### Case-insensitive matching

```lua
local re = cosmo.re

local match = re.search('ERROR', 'Warning: error occurred', re.ICASE)
print(match)  -- prints "error"

local html = re.search('<[Dd][Ii][Vv]>', '<DIV>content</DIV>', re.ICASE)
print(html)  -- prints "<DIV>"
```

### Extract numbers

```lua
local text = "The values are: 10, 20, 30"
local num = re.search('[0-9]+', text)
while num do
  print("Found: " .. num)
  -- To find all matches, you'd need to track position
  -- and search in remaining text
  break
end
```

### Validate input

```lua
local function is_valid_username(name)
  local match = re.search('^[a-z][a-z0-9_]*$', name)
  return match ~= nil
end

print(is_valid_username("alice"))      -- true
print(is_valid_username("bob_123"))    -- true
print(is_valid_username("123invalid")) -- false
print(is_valid_username("User"))       -- false
```

### Extract file extension

```lua
local filename = "document.pdf"
local ext = re.search('\\.[a-z]+$', filename, re.ICASE)
if ext then
  print("Extension: " .. ext)  -- prints "Extension: .pdf"
end
```

### Parse simple URLs

```lua
local url = "https://example.com/path"

-- Check if HTTPS
local is_https = re.search('^https://', url)
if is_https then
  print("Secure connection")
end

-- Extract domain
local domain = re.search('://([a-z.]+)/', url)
if domain then
  print("Domain: " .. domain)
end
```

## Notes

- The `search` function returns the first match only
- For multiple matches, you need to iterate through the text
- Backreferences like `\1` refer to capturing groups
- Use `\\` to match a literal backslash
- Character classes are locale-dependent
- Maximum subexpression depth may be limited

## Comparison with Lua patterns

POSIX regex vs Lua string patterns:

**POSIX regex advantages:**
- Standard regex syntax
- Character classes like `[:digit:]`
- Proper alternation with `|`
- Explicit quantifiers `{n,m}`

**Lua patterns advantages:**
- Simpler and faster
- Built into standard library
- Character classes like `%d`, `%w`
- Captures with `string.match()`

Choose POSIX regex when:
- Working with standard regex patterns
- Need complex alternation
- Interfacing with external tools

Choose Lua patterns when:
- Simple pattern matching
- Performance critical
- Want minimal dependencies
