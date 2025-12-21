# cosmo

Main cosmo module providing utility functions for encoding, compression, hashing, networking, and system information.

## Usage

```lua
local cosmo = require("cosmo")
```

The cosmo module includes several submodules:
- `cosmo.unix` - Unix system calls (see cosmo-unix.md)
- `cosmo.path` - Path manipulation (see cosmo-path.md)
- `cosmo.re` - Regular expressions (see cosmo-re.md)
- `cosmo.argon2` - Argon2 password hashing (see cosmo-argon2.md)
- `cosmo.sqlite3` - SQLite database (see cosmo-sqlite3.md)

## Encoding and decoding

### EncodeBase64

```lua
encoded = cosmo.EncodeBase64(data)
```

Encode binary data to base64 string.

```lua
local b64 = cosmo.EncodeBase64("hello world")
-- returns "aGVsbG8gd29ybGQ="
```

### DecodeBase64

```lua
data = cosmo.DecodeBase64(encoded)
```

Decode base64 string to binary data.

### EncodeBase32

```lua
encoded = cosmo.EncodeBase32(data)
```

Encode binary data to base32 string.

### DecodeBase32

```lua
data = cosmo.DecodeBase32(encoded)
```

Decode base32 string to binary data.

### EncodeHex

```lua
hex = cosmo.EncodeHex(data)
```

Encode binary data to hexadecimal string.

```lua
local hex = cosmo.EncodeHex("hello world")
-- returns "68656c6c6f20776f726c64"
```

### DecodeHex

```lua
data = cosmo.DecodeHex(hex)
```

Decode hexadecimal string to binary data.

### EncodeLatin1

```lua
encoded = cosmo.EncodeLatin1(data)
```

Encode data to Latin-1 encoding.

### DecodeLatin1

```lua
data = cosmo.DecodeLatin1(encoded)
```

Decode Latin-1 encoded data.

## Compression

### Deflate

```lua
compressed = cosmo.Deflate(data)
```

Compress data using DEFLATE algorithm.

```lua
local data = "hello world hello world hello world"
local compressed = cosmo.Deflate(data)
-- compressed is smaller than original
```

### Inflate

```lua
data = cosmo.Inflate(compressed, max_size)
```

Decompress DEFLATE data. `max_size` is the maximum output size allowed.

```lua
local decompressed = cosmo.Inflate(compressed, 1024)
```

### Compress

```lua
compressed = cosmo.Compress(data)
```

Compress data (zlib format).

### Uncompress

```lua
data = cosmo.Uncompress(compressed, max_size)
```

Decompress data (zlib format).

## Hashing and checksums

### Crc32

```lua
checksum = cosmo.Crc32(initial, data)
```

Compute CRC32 checksum. `initial` is usually `0` for first call.

```lua
local crc = cosmo.Crc32(0, "hello")
-- returns 907060870
```

### Crc32c

```lua
checksum = cosmo.Crc32c(initial, data)
```

Compute CRC32C checksum (Castagnoli polynomial).

### HighwayHash64

```lua
hash = cosmo.HighwayHash64(key, data)
```

Compute HighwayHash 64-bit hash.

### Lemur64

```lua
hash = cosmo.Lemur64(data)
```

Compute Lemur64 hash.

## URL and HTML escaping

### EscapeHtml

```lua
escaped = cosmo.EscapeHtml(text)
```

Escape HTML special characters.

```lua
local escaped = cosmo.EscapeHtml("<foo>")
-- returns "&lt;foo&gt;"
```

### EscapePath

```lua
escaped = cosmo.EscapePath(path)
```

URL-encode path component.

```lua
local escaped = cosmo.EscapePath("foo bar")
-- returns "foo%20bar"
```

### EscapeParam

```lua
escaped = cosmo.EscapeParam(value)
```

URL-encode query parameter value.

```lua
local escaped = cosmo.EscapeParam("a=b&c")
-- returns "a%3Db%26c"
```

### EscapeSegment

```lua
escaped = cosmo.EscapeSegment(segment)
```

URL-encode path segment.

### EscapeFragment

```lua
escaped = cosmo.EscapeFragment(fragment)
```

URL-encode fragment identifier.

### EscapeHost

```lua
escaped = cosmo.EscapeHost(host)
```

URL-encode hostname.

### EscapeUser

```lua
escaped = cosmo.EscapeUser(user)
```

URL-encode username.

### EscapePass

```lua
escaped = cosmo.EscapePass(password)
```

URL-encode password.

### EscapeLiteral

```lua
escaped = cosmo.EscapeLiteral(text)
```

Escape text for literal inclusion in URLs.

### EscapeIp

```lua
escaped = cosmo.EscapeIp(ip)
```

Format IP address for URLs.

## HTTP utilities

### GetHttpReason

```lua
reason = cosmo.GetHttpReason(status_code)
```

Get HTTP status reason phrase.

```lua
print(cosmo.GetHttpReason(200))  -- prints "OK"
print(cosmo.GetHttpReason(404))  -- prints "Not Found"
```

### IsValidHttpToken

```lua
bool = cosmo.IsValidHttpToken(token)
```

Test if string is valid HTTP token.

### IsHeaderRepeatable

```lua
bool = cosmo.IsHeaderRepeatable(header_name)
```

Test if HTTP header can appear multiple times.

### ParseHttpDateTime

```lua
timestamp = cosmo.ParseHttpDateTime(datestr)
```

Parse HTTP date string to Unix timestamp.

### ParseParams

```lua
params = cosmo.ParseParams(query_string)
```

Parse URL query parameters.

### ParseHost

```lua
host, port = cosmo.ParseHost(hostport)
```

Parse host and port from string.

## IP address utilities

### ParseIp

```lua
ip = cosmo.ParseIp(addr_string)
```

Parse IP address string to numeric form.

```lua
local ip = cosmo.ParseIp("192.168.1.1")
```

### FormatIp

```lua
addr_string = cosmo.FormatIp(ip)
```

Format numeric IP address to string.

```lua
local str = cosmo.FormatIp(ip)
-- returns "192.168.1.1"
```

### CategorizeIp

```lua
category = cosmo.CategorizeIp(ip)
```

Categorize IP address type.

### IsLoopbackIp

```lua
bool = cosmo.IsLoopbackIp(ip)
```

Test if IP address is loopback (127.0.0.0/8 or ::1).

```lua
local ip = cosmo.ParseIp("127.0.0.1")
print(cosmo.IsLoopbackIp(ip))  -- prints true
```

### IsPrivateIp

```lua
bool = cosmo.IsPrivateIp(ip)
```

Test if IP address is private (RFC 1918).

```lua
local ip = cosmo.ParseIp("192.168.1.1")
print(cosmo.IsPrivateIp(ip))  -- prints true
```

### IsPublicIp

```lua
bool = cosmo.IsPublicIp(ip)
```

Test if IP address is public (not private or loopback).

## Path validation

### IsAcceptableHost

```lua
bool = cosmo.IsAcceptableHost(hostname)
```

Test if hostname is acceptable.

### IsAcceptablePath

```lua
bool = cosmo.IsAcceptablePath(path)
```

Test if path is acceptable.

### IsAcceptablePort

```lua
bool = cosmo.IsAcceptablePort(port)
```

Test if port number is acceptable.

### IsReasonablePath

```lua
bool = cosmo.IsReasonablePath(path)
```

Test if path is reasonable (no suspicious patterns).

## Text utilities

### HasControlCodes

```lua
bool = cosmo.HasControlCodes(text)
```

Test if text contains control characters.

### VisualizeControlCodes

```lua
visualized = cosmo.VisualizeControlCodes(text)
```

Replace control codes with visual representations.

```lua
local text = "hello\x01world"
print(cosmo.VisualizeControlCodes(text))
-- prints "hello␁world"
```

### IndentLines

```lua
indented = cosmo.IndentLines(text, indent)
```

Indent each line of text.

### GetMonospaceWidth

```lua
width = cosmo.GetMonospaceWidth(text)
```

Get display width of text in monospace font.

### MeasureEntropy

```lua
entropy = cosmo.MeasureEntropy(data)
```

Measure Shannon entropy of data.

## System information

### GetHostOs

```lua
os_name = cosmo.GetHostOs()
```

Get operating system name.

```lua
print(cosmo.GetHostOs())  -- prints "LINUX", "DARWIN", "WINDOWS", etc.
```

### GetHostIsa

```lua
isa = cosmo.GetHostIsa()
```

Get instruction set architecture.

```lua
print(cosmo.GetHostIsa())  -- prints "X86_64", "AARCH64", etc.
```

### GetCpuCount

```lua
count = cosmo.GetCpuCount()
```

Get number of CPU cores.

### GetCpuCore

```lua
core = cosmo.GetCpuCore()
```

Get current CPU core number.

### GetCpuNode

```lua
node = cosmo.GetCpuNode()
```

Get current NUMA node.

## Time

### GetTime

```lua
seconds = cosmo.GetTime()
```

Get current time as floating-point seconds since epoch.

```lua
local t = cosmo.GetTime()
-- returns 1766271401.357
```

### Sleep

```lua
cosmo.Sleep(seconds)
```

Sleep for specified duration (supports fractional seconds).

```lua
cosmo.Sleep(0.5)  -- sleep for 500ms
```

## Random data

### GetRandomBytes

```lua
bytes = cosmo.GetRandomBytes(count)
```

Generate cryptographically secure random bytes.

```lua
local rand = cosmo.GetRandomBytes(16)
local hex = cosmo.EncodeHex(rand)
```

## Bit operations

### Bsf

```lua
position = cosmo.Bsf(value)
```

Bit scan forward - find position of least significant set bit.

```lua
print(cosmo.Bsf(16))  -- prints 4
```

### Bsr

```lua
position = cosmo.Bsr(value)
```

Bit scan reverse - find position of most significant set bit.

```lua
print(cosmo.Bsr(16))  -- prints 4
```

### Popcnt

```lua
count = cosmo.Popcnt(value)
```

Population count - count number of set bits.

```lua
print(cosmo.Popcnt(15))  -- prints 4 (binary 1111)
```

## Examples

### Encode and decode data

```lua
local data = "secret message"
local encoded = cosmo.EncodeBase64(data)
print("Encoded: " .. encoded)

local decoded = cosmo.DecodeBase64(encoded)
print("Decoded: " .. decoded)
```

### Compress and decompress

```lua
local text = string.rep("hello world ", 100)
local compressed = cosmo.Deflate(text)
print("Original: " .. #text .. " bytes")
print("Compressed: " .. #compressed .. " bytes")

local decompressed = cosmo.Inflate(compressed, #text)
assert(text == decompressed)
```

### Hash password with salt

```lua
local password = "user_password"
local salt = cosmo.GetRandomBytes(16)
local combined = salt .. password
local hash = cosmo.Crc32(0, combined)
```

### Validate IP address

```lua
local ip_str = "192.168.1.1"
local ip = cosmo.ParseIp(ip_str)

if ip then
  if cosmo.IsPrivateIp(ip) then
    print("Private IP address")
  elseif cosmo.IsLoopbackIp(ip) then
    print("Loopback address")
  elseif cosmo.IsPublicIp(ip) then
    print("Public IP address")
  end
end
```

### URL encoding

```lua
local params = {
  name = "John Doe",
  email = "john@example.com",
  message = "Hello & goodbye"
}

local query_parts = {}
for k, v in pairs(params) do
  table.insert(query_parts, k .. "=" .. cosmo.EscapeParam(v))
end

local query_string = table.concat(query_parts, "&")
```
