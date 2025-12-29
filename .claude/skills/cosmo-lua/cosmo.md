# cosmo

### AcquireToken

```lua
AcquireToken(ip)
```

Atomically acquires token.
This routine atomically acquires a single token for an `ip` address.
The return value is the token count before the subtraction happened.
No action is taken based on the count, since the caller will decide.
`ip` should be an IPv4 address and this defaults to `GetClientAddr()`,
although other interpretations of its meaning are possible.
Your token buckets are stored in shared memory so this can be called
from multiple forked processes. which operate on the same values.

**Parameters:**

- `ip` (uint32?)

**Returns:**

- `int8`

### Barf

```lua
Barf(filename, data, mode, flags, offset)
```

Writes all data to file the easy way.
This function writes to the local file system.
For example:
assert(Barf('x.txt', 'abc123'))
assert(Barf('x.txt', 'XX', 0, 0, 3))
assert(assert(Slurp('x.txt', 1, 6)) == 'abXX23')

**Parameters:**

- `filename` (string)
- `data` (string)
- `mode` (integer?): defaults to 0644. This parameter is ignored when flags doesn't have `unix.O_CREAT`.
- `flags` (integer?): defaults to `unix.O_TRUNC | unix.O_CREAT`.
- `offset` (integer?): is 1-indexed and may be used to overwrite arbitrary slices within a file when used in conjunction with `flags=0`.

**Returns:**

- `true`

### Benchmark

```lua
Benchmark(func, count, maxattempts)
```

Performs microbenchmark. Nanoseconds are computed from RDTSC tick counts,
using an approximation that's measured beforehand with the
unix.`clock_gettime()` function. The `ticks` result is the canonical average
number of clock ticks. This subroutine will subtract whatever the overhead
happens to be for benchmarking a function that does nothing. This overhead
value will be reported in the result. `tries` indicates if your microbenchmark
needed to be repeated, possibly because your system is under load and the
benchmark was preempted by the operating system, or moved to a different core.

**Parameters:**

- `func` (function)
- `count` (integer?)
- `maxattempts` (integer?)

**Returns:**

- `number`: nanoseconds the average number of nanoseconds that `func` needed to execute
- `integer`: ticks
- `integer`: overhead_ticks
- `integer`: tries

### Blackhole

```lua
Blackhole(ip)
```

Sends IP address to blackholed service.
`ProgramTokenBucket()` needs to be called beforehand. The default
settings will blackhole automatically, during the `accept()` loop
based on the banned threshold. However if your Lua code calls
`AcquireToken()` manually, then you'll need this function to take
action on the returned values.
This function returns true if a datagram could be sent sucessfully.
Otherwise false is returned, which can happen if blackholed isn't
running, or if a lot of processes are sending messages to it and the
operation would have blocked.
It's assumed that the blackholed service is running locally in the
background.

**Parameters:**

- `ip` (uint32)

### Bsf

```lua
Bsf(x)
```

Passing `0` will raise an error. Same as the Intel x86 instruction BSF.

**Parameters:**

- `x` (integer)

**Returns:**

- `integer`: # position of first bit set.

### Bsr

```lua
Bsr(x)
```

Passing `0` will raise an error. Same as the Intel x86 instruction BSR.

**Parameters:**

- `x` (integer)

**Returns:**

- `integer`: # binary logarithm of `x`

### CategorizeIp

```lua
CategorizeIp(ip)
```

**Parameters:**

- `ip` (uint32)

**Returns:**

- `string`: # a string describing the IP address. This is currently Class A granular. It can tell you if traffic originated from private networks, ARIN, APNIC, DOD, etc.

### CountTokens

```lua
CountTokens(ip)
```

Counts number of tokens in bucket.
This function is the same as AcquireToken() except no subtraction is
performed, i.e. no token is taken.
`ip` should be an IPv4 address and this defaults to GetClientAddr(),
although other interpretations of its meaning are possible.

**Parameters:**

- `ip` (uint32?)

**Returns:**

- `int8`

### Crc32

```lua
Crc32(initial, data)
```

Computes Phil Katz CRC-32 used by zip/zlib/gzip/etc.

**Parameters:**

- `initial` (integer)
- `data` (string)

**Returns:**

- `integer`

### Crc32c

```lua
Crc32c(initial, data)
```

Computes 32-bit Castagnoli Cyclic Redundancy Check.

**Parameters:**

- `initial` (integer)
- `data` (string)

**Returns:**

- `integer`

### Decimate

```lua
Decimate(data)
```

Shrinks byte buffer in half using John Costella's magic kernel. This downscales
data 2x using an eight-tap convolution, e.g.
>: Decimate('\xff\xff\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00')
"\xff\x00\xff\x00\xff\x00"
This is very fast if SSSE3 is available (Intel 2004+ / AMD 2011+).

**Parameters:**

- `data` (string)

**Returns:**

- `string`

### DecodeBase64

```lua
DecodeBase64(ascii)
```

Decodes binary data encoded as base64.
This turns ASCII into binary, in a permissive way that ignores
characters outside the base64 alphabet, such as whitespace. See
`decodebase64.c`.

**Parameters:**

- `ascii` (string)

**Returns:**

- `string`: binary

### DecodeHex

```lua
DecodeHex(ascii)
```

Turns ASCII base-16 hexadecimal byte string into binary string,
case-insensitively. Non-hex characters may not appear in string.

**Parameters:**

- `ascii` (string)

**Returns:**

- `string`: binary

### DecodeJson

```lua
DecodeJson(input)
```

Turns JSON string into a Lua data structure.
This is a generally permissive parser, in the sense that like
v8, it permits scalars as top-level values. Therefore we must
note that this API can be thought of as special, in the sense
val = assert(DecodeJson(str))
will usually do the right thing, except in cases where `false`
or `null` are the top-level value. In those cases, it's needed
to check the second value too in order to discern from error
val, err = DecodeJson(str)
if not val then
if err then
print('bad json', err)
elseif val == nil then
print('val is null')
elseif val == false then
print('val is false')
end
end
This parser supports 64-bit signed integers. If an overflow
happens, then the integer is silently coerced to double, as
consistent with v8. If a double overflows into `Infinity`, we
coerce it to `null` since that's what v8 does, and the same
goes for underflows which, like v8, are coerced to `0.0`.
When objects are parsed, your Lua object can't preserve the
original ordering of fields. As such, they'll be sorted by
`EncodeJson()` and may not round-trip with original intent.
This parser has perfect conformance with JSONTestSuite.
This parser validates utf-8 and utf-16.

**Parameters:**

- `input` (string)

**Returns:**

- `JsonValue`

### DecodeLatin1

```lua
DecodeLatin1(iso_8859_1)
```

Turns ISO-8859-1 string into UTF-8.

**Parameters:**

- `iso_8859_1` (string)

**Returns:**

- `string`: UTF8

### Deflate

```lua
Deflate(uncompressed, level)
```

Compresses data.
>: Deflate("hello")
"\xcbH\xcd\xc9\xc9\x07\x00"
>: Inflate("\xcbH\xcd\xc9\xc9\x07\x00", 5)
"hello"
The output format is raw DEFLATE that's suitable for embedding into formats
like a ZIP file. It's recommended that, like ZIP, you also store separately a
`Crc32()` checksum in addition to the original uncompressed size.
Lower numbers go faster (4 for instance is a sweet spot) and higher numbers go
slower but have better compression.

**Parameters:**

- `uncompressed` (string)
- `level` (integer?): the compression level, which defaults to `7`. The max is `9`.

**Returns:**

- `string`: compressed

### EncodeBase64

```lua
EncodeBase64(binary)
```

Turns binary into ASCII. This can be used to create HTML data:
URIs that do things like embed a PNG file in a web page. See
encodebase64.c.

**Parameters:**

- `binary` (string)

**Returns:**

- `string`: ascii

### EncodeHex

```lua
EncodeHex(binary)
```

Turns binary into ASCII base-16 hexadecimal lowercase string.

**Parameters:**

- `binary` (string)

**Returns:**

- `string`: ascii

### EncodeJson

```lua
EncodeJson(value, options)
```

Turns Lua data structure into JSON string.
Since Lua uses tables are both hashmaps and arrays, we use a
simple fast algorithm for telling the two apart. Tables with
non-zero length (as reported by `#`) are encoded as arrays,
and any non-array elements are ignored. For example:
>: EncodeJson({2})
"[2]"
>: EncodeJson({[1]=2, ["hi"]=1})
"[2]"
If there are holes in your array, then the serialized array
will exclude everything after the first hole. If the beginning
of your array is a hole, then an error is returned.
>: EncodeJson({[1]=1, [3]=3})
"[1]"
>: EncodeJson({[2]=1, [3]=3})
"[]"
>: EncodeJson({[2]=1, [3]=3})
nil     "json objects must only use string keys"
If the raw length of a table is reported as zero, then we
check for the magic element `[0]=false`. If it's present, then
your table will be serialized as empty array `[]`. An entry is
inserted by `DecodeJson()` automatically, only when encountering
empty arrays, and it's necessary in order to make empty arrays
round-trip. If raw length is zero and `[0]=false` is absent,
then your table will be serialized as an iterated object.
>: EncodeJson({})
"{}"
>: EncodeJson({[0]=false})
"[]"
>: EncodeJson({["hi"]=1})
"{\"hi\":1}"
>: EncodeJson({["hi"]=1, [0]=false})
"[]"
>: EncodeJson({["hi"]=1, [7]=false})
nil     "json objects must only use string keys"
The following options may be used:
- `useoutput`: `(bool=false)` encodes the result directly to the output buffer
and returns nil value. This option is ignored if used outside of request
handling code.
- `sorted`: `(bool=true)` Lua uses hash tables so the order of object keys is
lost in a Lua table. So, by default, we use strcmp to impose a deterministic
output order. If you don't care about ordering then setting `sorted=false`
should yield a performance boost in serialization.
- `pretty`: `(bool=false)` Setting this option to true will cause tables with
more than one entry to be formatted across multiple lines for readability.
- `indent`: `(str=" ")` This option controls the indentation of pretty
formatting. This field is ignored if pretty isn't `true`.
- `maxdepth`: `(int=64)` This option controls the maximum amount of recursion
the serializer is allowed to perform. The max is 32767. You might not be able
to set it that high if there isn't enough C stack memory. Your serializer
checks for this and will return an error rather than crashing.
If the raw length of a table is reported as zero, then we
check for the magic element `[0]=false`. If it's present, then
your table will be serialized as empty array `[]`. An entry is
inserted by `DecodeJson()` automatically, only when encountering
empty arrays, and it's necessary in order to make empty arrays
round-trip. If raw length is zero and `[0]=false` is absent,
then your table will be serialized as an iterated object.
This function will return an error if:
- value is cyclic
- value has depth greater than 64
- value contains functions, user data, or threads
- value is table that blends string / non-string keys
- Your serializer runs out of C heap memory (setrlimit)
We assume strings in value contain UTF-8. This serializer currently does not
produce UTF-8 output. The output format is right now ASCII. Your UTF-8 data
will be safely transcoded to `\uXXXX` sequences which are UTF-16. Overlong
encodings in your input strings will be canonicalized rather than validated.
NaNs are serialized as `null` and Infinities are `null` which is consistent
with the v8 behavior.

**Parameters:**

- `value` (JsonValue)
- `options` ({): useoutput: false?, sorted: boolean?, pretty: boolean?, indent: string?, maxdepth: integer? }?

**Returns:**

- `string`

### EncodeLatin1

```lua
EncodeLatin1(utf8, flags)
```

Turns UTF-8 into ISO-8859-1 string.

**Parameters:**

- `utf8` (string)
- `flags` (integer)

**Returns:**

- `string`: iso_8859_1

### EncodeLua

```lua
EncodeLua(value, options)
```

Turns Lua data structure into Lua code string.
Since Lua uses tables as both hashmaps and arrays, tables will only be
serialized as an array with determinate order, if it's an array in the
strictest possible sense.
1. for all 𝑘=𝑣 in table, 𝑘 is an integer ≥1
2. no holes exist between MIN(𝑘) and MAX(𝑘)
3. if non-empty, MIN(𝑘) is 1
In all other cases, your table will be serialized as an object which is
iterated and displayed as a list of (possibly) sorted entries that have
equal signs.
>: EncodeLua({3, 2})
"{3, 2}"
>: EncodeLua({[1]=3, [2]=3})
"{3, 2}"
>: EncodeLua({[1]=3, [3]=3})
"{[1]=3, [3]=3}"
>: EncodeLua({["hi"]=1, [1]=2})
"{[1]=2, hi=1}"
The following options may be used:
- `useoutput`: `(bool=false)` encodes the result directly to the output buffer
and returns nil value. This option is ignored if used outside of request
handling code.
- `sorted`: `(bool=true)` Lua uses hash tables so the order of object keys is
lost in a Lua table. So, by default, we use strcmp to impose a deterministic
output order. If you don't care about ordering then setting `sorted=false`
should yield a performance boost in serialization.
- `pretty`: `(bool=false)` Setting this option to true will cause tables with
more than one entry to be formatted across multiple lines for readability.
- `indent`: `(str=" ")` This option controls the indentation of pretty
formatting. This field is ignored if pretty isn't `true`.
- `maxdepth`: `(int=64)` This option controls the maximum amount of recursion
the serializer is allowed to perform. The max is 32767. You might not be able
to set it that high if there isn't enough C stack memory. Your serializer
checks for this and will return an error rather than crashing.
If a user data object has a `__repr` or `__tostring` meta method, then that'll
be used to encode the Lua code.
This serializer is designed primarily to describe data. For example, it's used
by the REPL where we need to be able to ignore errors when displaying data
structures, since showing most things imperfectly is better than crashing.
Therefore this isn't the kind of serializer you'd want to use to persist data
in prod. Try using the JSON serializer for that purpose.
Non-encodable value types (e.g. threads, functions) will be represented as a
string literal with the type name and pointer address. The string description
is of an unspecified format that could most likely change. This encoder detects
cyclic tables; however instead of failing, it embeds a string of unspecified
layout describing the cycle.
Integer literals are encoded as decimal. However if the int64 number is ≥256
and has a population count of 1 then we switch to representating the number in
hexadecimal, for readability. Hex numbers have leading zeroes added in order
to visualize whether the number fits in a uint16, uint32, or int64. Also some
numbers can only be encoded expressionally. For example, `NaN`s are serialized
as `0/0`, and `Infinity` is `math.huge`.
>: 7000
7000
>: 0x100
0x0100
>: 0x10000
0x00010000
>: 0x100000000
0x0000000100000000
>: 0/0
0/0
>: 1.5e+9999
math.huge
>: -9223372036854775807 - 1
-9223372036854775807 - 1
The only failure return condition currently implemented is when C runs out of heap memory.

**Parameters:**

- `options` ({): useoutput: false?, sorted: boolean?, pretty: boolean?, indent: string?, maxdepth: integer? }?

**Returns:**

- `string`

### EncodeUrl

```lua
EncodeUrl(url)
```

This function is the inverse of ParseUrl. The output will always be correctly
formatted. The exception is if illegal characters are supplied in the scheme
field, since there's no way of escaping those. Opaque parts are escaped as
though they were paths, since many URI parsers won't understand things like
an unescaped question mark in path.

**Parameters:**

- `url` (Url)

**Returns:**

- `string`: url

### EscapeFragment

```lua
EscapeFragment(str)
```

Escapes URL #fragment. The allowed characters are `-/?.~_@:!$&'()*+,;=0-9A-Za-z`
and everything else gets `%XX` encoded. Please note that `'&` can still break
HTML and that `'()` can still break CSS URLs. This function is charset agnostic
and will not canonicalize overlong encodings. It is assumed that a UTF-8 string
will be supplied. See `kescapefragment.S`.

**Parameters:**

- `str` (string)

**Returns:**

- `string`

### EscapeHost

```lua
EscapeHost(str)
```

Escapes URL host. See `kescapeauthority.S`.

**Parameters:**

- `str` (string)

**Returns:**

- `string`

### EscapeHtml

```lua
EscapeHtml(str)
```

Escapes HTML entities: The set of entities is `&><"'` which become `&amp;&gt;&lt;&quot;&#39;`. This function is charset agnostic and will not canonicalize overlong encodings. It is assumed that a UTF-8 string will be supplied. See `escapehtml.c`.

**Parameters:**

- `str` (string)

**Returns:**

- `string`

### EscapeLiteral

```lua
EscapeLiteral(str)
```

Escapes JavaScript or JSON string literal content. The caller is responsible
for adding the surrounding quotation marks. This implementation \uxxxx sequences
for all non-ASCII sequences. HTML entities are also encoded, so the output
doesn't need `EscapeHtml`. This function assumes UTF-8 input. Overlong
encodings are canonicalized. Invalid input sequences are assumed to
be ISO-8859-1. The output is UTF-16 since that's what JavaScript uses. For
example, some individual codepoints such as emoji characters will encode as
multiple `\uxxxx` sequences. Ints that are impossible to encode as UTF-16 are
substituted with the `\xFFFD` replacement character.
See `escapejsstringliteral.c`.

**Parameters:**

- `str` (string)

**Returns:**

- `string`

### EscapeParam

```lua
EscapeParam(str)
```

Escapes URL parameter name or value. The allowed characters are `-.*_0-9A-Za-z`
and everything else gets `%XX` encoded. This function is charset agnostic and
will not canonicalize overlong encodings. It is assumed that a UTF-8 string
will be supplied. See `kescapeparam.S`.

**Parameters:**

- `str` (string)

**Returns:**

- `string`

### EscapePass

```lua
EscapePass(str)
```

Escapes URL password. See `kescapeauthority.S`.

**Parameters:**

- `str` (string)

**Returns:**

- `string`

### EscapePath

```lua
EscapePath(str)
```

Escapes URL path. This is the same as EscapeSegment except slash is allowed.
The allowed characters are `-.~_@:!$&'()*+,;=0-9A-Za-z/` and everything else
gets `%XX` encoded. Please note that `'&` can still break HTML, so the output
may need EscapeHtml too. Also note that `'()` can still break CSS URLs. This
function is charset agnostic and will not canonicalize overlong encodings.
It is assumed that a UTF-8 string will be supplied. See `kescapepath.S`.

**Parameters:**

- `str` (string)

**Returns:**

- `string`

### EscapeSegment

```lua
EscapeSegment(str)
```

Escapes URL path segment. This is the same as EscapePath except slash isn't
allowed. The allowed characters are `-.~_@:!$&'()*+,;=0-9A-Za-z` and everything
else gets `%XX` encoded. Please note that `'&` can still break HTML, so the
output may need EscapeHtml too. Also note that `'()` can still break CSS URLs.
This function is charset agnostic and will not canonicalize overlong encodings.
It is assumed that a UTF-8 string will be supplied. See `kescapesegment.S`.

**Parameters:**

- `str` (string)

**Returns:**

- `string`

### EscapeUser

```lua
EscapeUser(str)
```

Escapes URL username. See `kescapeauthority.S`.

**Parameters:**

- `str` (string)

**Returns:**

- `string`

### EvadeDragnetSurveillance

```lua
EvadeDragnetSurveillance(bool)
```

If this option is programmed then redbean will not transmit a Server Name
Indicator (SNI) when performing `Fetch()` requests. This function is not
available in unsecure mode.

**Parameters:**

- `bool` (boolean)

### Fetch

```lua
Fetch(url, body)
```

Sends an HTTP/HTTPS request to the specified URL. If only the URL is provided,
then a GET request is sent. If both URL and body parameters are specified, then
a POST request is sent. If any other method needs to be specified (for example,
PUT or DELETE), then passing a table as the second value allows setting method
and body values as well other options:
- `method` (default: `"GET"`): sets the method to be used for the request.
The specified method is converted to uppercase.
- `body` (default: `""`): sets the body value to be sent.
- `followredirect` (default: `true`): forces temporary and permanent redirects
to be followed. This behavior can be disabled by passing `false`.
- `maxredirects` (default: `5`): sets the number of allowed redirects to
minimize looping due to misconfigured servers. When the number is exceeded,
the result of the last redirect is returned.
- `keepalive` (default = `false`): configures each request to keep the
connection open (unless closed by the server) and reuse for the
next request to the same host. This option is disabled when SSL
connection is used.
The mapping of hosts and their sockets is stored in a table
assigned to the `keepalive` field itself, so it can be passed to
the next call.
If the table includes the `close` field set to a true value,
then the connection is closed after the request is made and the
host is removed from the mapping table.
- `proxy` (string): HTTP proxy URL, e.g. `"http://proxy:8080"`.
Supports Basic authentication: `"http://user:pass@proxy:8080"`.
- `maxresponse` (default: `104857600`): maximum response size in bytes.
Protects against memory exhaustion from large responses.
- `resettls` (default: `true`): reset TLS state after fork.
Ensures child processes get fresh DRBG entropy.
Environment variables:
- `http_proxy` / `HTTP_PROXY`: default proxy URL when `proxy` option
is not specified. Supports same format as the option.
- `SSL_CERT_FILE`: path to CA certificate bundle file for TLS verification.
Overrides default system CA locations.
- `SSL_NO_SYSTEM_CERTS`: if set, skip loading system CA certificates.
Only embedded certificates will be used.
When the redirect is being followed, the same method and body values are being
sent in all cases except when 303 status is returned. In that case the method
is set to GET and the body is removed before the redirect is followed. Note
that if these (method/body) values are provided as table fields, they will be
modified in place.

**Parameters:**

- `url` (string)
- `body` (string|{) *(optional)*: headers: table<string,string>, method: string, body: string, maxredirects: integer?, keepalive: boolean?, proxy: string?, maxresponse: integer?, resettls: boolean? }

**Returns:**

- `integer`: status, table<string,string> headers, string body

### FormatHttpDateTime

```lua
FormatHttpDateTime(seconds)
```

Converts UNIX timestamp to an RFC1123 string that looks like this:
`Mon, 29 Mar 2021 15:37:13 GMT`. See `formathttpdatetime.c`.

**Parameters:**

- `seconds` (integer)

**Returns:**

- `string`

### FormatIp

```lua
FormatIp(uint32)
```

Turns integer like `0x01020304` into a string like `"1.2.3.4"`. See also
`ParseIp` for the inverse operation.

**Parameters:**

- `uint32` (integer)

**Returns:**

- `string`

### GetAssetComment

```lua
GetAssetComment(path)
```

**Parameters:**

- `path` (string)

**Returns:**

- `string?`: comment comment text associated with asset in the ZIP central directory.

### GetAssetLastModifiedTime

```lua
GetAssetLastModifiedTime(path)
```

**Parameters:**

- `path` (string)

**Returns:**

- `number`: seconds UNIX timestamp for modification time of a ZIP asset (or local file if the -D flag is used). If both a file and a ZIP asset are present, then the file is used.

### GetAssetMode

```lua
GetAssetMode(path)
```

**Parameters:**

- `path` (string)

**Returns:**

- `integer`: mode UNIX-style octal mode for ZIP asset (or local file if the -D flag is used)

### GetAssetSize

```lua
GetAssetSize(path)
```

**Parameters:**

- `path` (string)

**Returns:**

- `integer`: bytesize byte size of uncompressed contents of ZIP asset (or local file if the `-D` flag is used)

### GetBody

```lua
GetBody()
```

**Returns:**

- `string`: body the request message body if present or an empty string.

### GetClientAddr

```lua
GetClientAddr()
```

Returns client socket ip4 address and port, e.g. `0x01020304,31337` would
represent `1.2.3.4:31337`. Please consider using `GetRemoteAddr` instead,
since the latter takes into consideration reverse proxy scenarios.

**Returns:**

- `uint32`: ip uint32
- `uint16`: port uint16

### GetClientFd

```lua
GetClientFd()
```

**Returns:**

- `integer`: clientfd

### GetComment

```lua
GetComment(path)
```

**Parameters:**

- `path` (string)

**Returns:**

- `string?`

### GetCookie

```lua
GetCookie(name)
```

**Parameters:**

- `name` (string)

**Returns:**

- `string`: cookie

### GetCpuCore

```lua
GetCpuCore()
```

**Returns:**

- `integer`: # 0-indexed CPU core on which process is currently scheduled.

### GetCpuCount

```lua
GetCpuCount()
```

**Returns:**

- `integer`: cpucount CPU core count or `0` if it couldn't be determined.

### GetCpuNode

```lua
GetCpuNode()
```

**Returns:**

- `integer`: # 0-indexed NUMA node on which process is currently scheduled.

### GetCryptoHash

```lua
GetCryptoHash(name, payload, key)
```

**Parameters:**

- `name` ("MD5"|"SHA1"|"SHA224"|"SHA256"|"SHA384"|"SHA512"|"BLAKE2B256")
- `payload` (string)
- `key` (string?): If the key is provided, then HMAC value of the same function is returned.

**Returns:**

- `string`: # value of the specified cryptographic hash function.

### GetDate

```lua
GetDate()
```

Date header, which is now, give or take a second. The returned value is a UNIX
timestamp.

**Returns:**

- `integer`: unixts date associated with request that's used to generate the

### GetEffectivePath

```lua
GetEffectivePath()
```

**Returns:**

- `string`: path as it was resolved by the routing algorithms, which might contain the virtual host prepended if used.

### GetFragment

```lua
GetFragment()
```

**Returns:**

- `string?`

### GetHeader

```lua
GetHeader(name)
```

Returns HTTP header. name is case-insensitive. The header value is returned as
a canonical UTF-8 string, with leading and trailing whitespace trimmed, which
was decoded from ISO-8859-1, which is guaranteed to not have C0/C1 control
sequences, with the exception of the tab character. Leading and trailing
whitespace is automatically removed. In the event that the client suplies raw
UTF-8 in the HTTP message headers, the original UTF-8 sequence can be
losslessly restored by counter-intuitively recoding the returned string back
to Latin1. If the requested header is defined by the RFCs as storing
comma-separated values (e.g. Allow, Accept-Encoding) and the field name occurs
multiple times in the message, then this function will fold those multiple
entries into a single string.

**Parameters:**

- `name` (string)

**Returns:**

- `string`

### GetHeaders

```lua
GetHeaders()
```

Returns HTTP headers as dictionary mapping header key strings to their UTF-8
decoded values. The ordering of headers from the request message is not
preserved. Whether or not the same key can repeat depends on whether or not
it's a standard header, and if so, if it's one of the ones that the RFCs
define as repeatable. See `khttprepeatable.c`. Those headers will not be
folded. Standard headers which aren't on that list, will be overwritten with
the last-occurring one during parsing. Extended headers are always passed
through exactly as they're received. Please consider using `GetHeader` API if
possible since it does a better job abstracting these issues.

**Returns:**

- `table<string,`: string>

### GetHost

```lua
GetHost()
```

**Returns:**

- `string`: # host associated with request. This will be the Host header, if it's supplied. Otherwise it's the bind address.

### GetHostIsa

```lua
GetHostIsa()
```

Returns string describing host instruction set architecture.
This can return:
- `"X86_64"` for Intel and AMD systems
- `"AARCH64"` for ARM64, M1, and Raspberry Pi systems
- `"POWERPC64"` for OpenPOWER Raptor Computing Systems

**Returns:**

- `"X86_64"|"AARCH64"|"POWERPC64"`

### GetHostOs

```lua
GetHostOs()
```

**Returns:**

- `"LINUX"|"METAL"|"WINDOWS"|"XNU"|"NETBSD"|"FREEBSD"|"OPENBSD"`: osname string that describes the host OS.

### GetHttpReason

```lua
GetHttpReason(code)
```

**Parameters:**

- `code` (integer)

**Returns:**

- `string`: reason string describing the HTTP reason phrase. See `gethttpreason.c`.

### GetHttpVersion

```lua
GetHttpVersion()
```

**Returns:**

- `integer`: httpversion the request HTTP protocol version, which can be `9` for `HTTP/0.9`, `10` for `HTTP/1.0`, or `11` for `HTTP/1.1`.

### GetLastModifiedTime

```lua
GetLastModifiedTime(path)
```

**Parameters:**

- `path` (string)

**Returns:**

- `number`

### GetLogLevel

```lua
GetLogLevel()
```

Returns logger verbosity level. Likely return values are `kLogDebug` >
`kLogVerbose` > `kLogInfo` > `kLogWarn` > `kLogError` > `kLogFatal`.

**Returns:**

- `integer`

### GetMethod

```lua
GetMethod()
```

Normally this will be GET, HEAD, or POST in which case redbean normalizes this
value to its uppercase form. Anything else that the RFC classifies as a "token"
string is accepted too, which might contain characters like &".

**Returns:**

- `string`: method HTTP method.

### GetMonospaceWidth

```lua
GetMonospaceWidth(str)
```

This is useful for fixed-width formatting. For example, CJK characters
typically take up two cells. This function takes into consideration combining
characters, which are discounted, as well as control codes and ANSI escape
sequences.

**Parameters:**

- `str` (string|integer): monospace display width of string.

**Returns:**

- `integer`

### GetParam

```lua
GetParam(name)
```

Returns first value associated with name. name is handled in a case-sensitive manner. This function checks Request-URL parameters first. Then it checks `application/x-www-form-urlencoded` from the message body, if it exists, which is common for HTML forms sending `POST` requests. If a parameter is supplied matching name that has no value, e.g. `foo` in `?foo&bar=value`, then the returned value will be `nil`, whereas for `?foo=&bar=value` it would be `""`. To differentiate between no-equal and absent, use the `HasParam` function. The returned value is decoded from ISO-8859-1 (only in the case of Request-URL) and we assume that percent-encoded characters were supplied by the client as UTF-8 sequences, which are returned exactly as the client supplied them, and may therefore may contain overlong sequences, control codes, `NUL` characters, and even numbers which have been banned by the IETF. It is the responsibility of the caller to impose further restrictions on validity, if they're desired.

**Parameters:**

- `name` (string)

**Returns:**

- `string`: value

### GetParams

```lua
GetParams()
```

This may contain duplicates. The inner array will have either one or two items,
depending on whether or not the equals sign was used.

**Returns:**

- `{`: [1]: string, [2]: string? }[] # name=value parameters from Request-URL and `application/x-www-form-urlencoded` message body in the order they were received.

### GetPass

```lua
GetPass()
```

**Returns:**

- `string?`: pass

### GetPath

```lua
GetPath()
```

This is guaranteed to begin with `"/"` It is further guaranteed that no `"//"`
or `"/."` exists in the path. The returned value is returned as a UTF-8 string
which was decoded from ISO-8859-1. We assume that percent-encoded characters
were supplied by the client as UTF-8 sequences, which are returned exactly as
the client supplied them, and may therefore may contain overlong sequences,
control codes, `NUL` characters, and even numbers which have been banned by
the IETF. redbean takes those things into consideration when performing path
safety checks. It is the responsibility of the caller to impose further
restrictions on validity, if they're desired.

**Returns:**

- `string`: path the Request-URL path.

### GetPayload

```lua
GetPayload()
```

**Returns:**

- `string`

### GetPort

```lua
GetPort()
```

**Returns:**

- `uint16`: port

### GetRandomBytes

```lua
GetRandomBytes(length)
```

**Parameters:**

- `length` (integer?)

**Returns:**

- `string`: # with the specified number of random bytes (1..256). If no length is specified, then a string of length 16 is returned.

### GetRedbeanVersion

```lua
GetRedbeanVersion()
```

**Returns:**

- `integer`: redbeanversion the Redbean version in the format 0xMMmmpp, with major (MM), minor (mm), and patch (pp) versions encoded. The version value 1.4 would be represented as 0x010400.

### GetRemoteAddr

```lua
GetRemoteAddr()
```

Returns client ip4 address and port, e.g. `0x01020304`,`31337` would represent
`1.2.3.4:31337`. This is the same as `GetClientAddr` except it will use the
ip:port from the `X-Forwarded-For` header, only if `IsPrivateIp` or
`IsPrivateIp` returns `true`.

**Returns:**

- `integer`: ip, integer port uint32 and uint16 respectively

### GetResponseBody

```lua
GetResponseBody()
```

returns an empty string during streaming.

**Returns:**

- `string`

### GetScheme

```lua
GetScheme()
```

**Returns:**

- `string?`: scheme from Request-URL, if any.

### GetServerAddr

```lua
GetServerAddr()
```

Returns address to which listening server socket is bound, e.g.
`0x01020304,8080` would represent `1.2.3.4:8080`. If `-p 0` was supplied as
the listening port, then the port in this string will be whatever number the
operating system assigned.

**Returns:**

- `uint32`: ip uint32
- `uint16`: port uint16

### GetStatus

```lua
GetStatus()
```

**Returns:**

- `integer?`: current status (as set by an earlier `SetStatus` call) or `nil` if the status hasn't been set yet.

### GetTime

```lua
GetTime()
```

**Returns:**

- `number`: seconds current time as a UNIX timestamp with 0.0001s precision.

### GetUrl

```lua
GetUrl()
```

Illegal characters or UTF-8 is guaranteed to be percent encoded, and has been
normalized to include either the Host or `X-Forwarded-Host` headers, if they
exist, and possibly a scheme too if redbean is being used as an HTTP proxy
server.
In the future this API might change to return an object instead.

**Returns:**

- `string`: url the effective Request-URL as an ASCII string

### GetUser

```lua
GetUser()
```

**Returns:**

- `string?`: user

### GetVersion

```lua
GetVersion()
```

**Returns:**

- `integer`

### GetZipPaths

```lua
GetZipPaths(prefix)
```

If prefix parameter is provided, then only paths that start with the prefix
(case sensitive) are returned.

**Parameters:**

- `prefix` (string?): paths of all assets in the zip central directory, prefixed by a slash.

**Returns:**

- `string[]`

### HasParam

```lua
HasParam(name)
```

**Parameters:**

- `name` (string)

**Returns:**

- `boolean`: # `true` if parameter with name was supplied in either the Request-URL or an application/x-www-form-urlencoded message body.

### HidePath

```lua
HidePath(prefix)
```

Programs redbean `/` listing page to not display any paths beginning with prefix.
This function should only be called from `/.init.lua`.

**Parameters:**

- `prefix` (string)

### IndentLines

```lua
IndentLines(str, int)
```

Adds spaces to beginnings of multiline string. If the int parameter is not
supplied then 1 space will be added.

**Parameters:**

- `str` (string)
- `int` (integer?)

**Returns:**

- `string`

### Inflate

```lua
Inflate(compressed, maxoutsize)
```

Decompresses data.
This function performs the inverse of Deflate(). It's recommended that you
perform a `Crc32()` check on the output string after this function succeeds.
However, it is permissable (although not advised) to specify some large number
in which case (on success) the byte length of the output string may be less
than `maxoutsize`.

**Parameters:**

- `compressed` (string)
- `maxoutsize` (integer): the uncompressed size, which should be known.

**Returns:**

- `string`: uncompressed

### IsAcceptablePath

```lua
IsAcceptablePath(str)
```

**Parameters:**

- `str` (string)

**Returns:**

- `boolean`: # `true` if path doesn't contain ".", ".." or "//" segments See `isacceptablepath.c`

### IsAssetCompressed

```lua
IsAssetCompressed(path)
```

**Parameters:**

- `path` (string)

**Returns:**

- `boolean`: # `true` if ZIP artifact at path is stored on disk using DEFLATE compression.

### IsClientUsingSsl

```lua
IsClientUsingSsl()
```

**Returns:**

- `boolean`

### IsCompressed

```lua
IsCompressed(path)
```

**Parameters:**

- `path` (string)

**Returns:**

- `boolean`

### IsDaemon

```lua
IsDaemon()
```

Returns `true` if `-d` flag was passed to redbean.

**Returns:**

- `boolean`

### IsHiddenPath

```lua
IsHiddenPath(path)
```

**Parameters:**

- `path` (string)

**Returns:**

- `boolean`: # `true` if the prefix of the given path is set with `HidePath`.

### IsLoopbackClient

```lua
IsLoopbackClient()
```

**Returns:**

- `boolean`: # `true` if the client IP address (returned by GetRemoteAddr) is part of the localhost network (127.0.0.0/8).

### IsLoopbackIp

```lua
IsLoopbackIp(uint32)
```

**Parameters:**

- `uint32` (integer)

**Returns:**

- `boolean`: # true if IP address is part of the localhost network (127.0.0.0/8).

### IsPrivateIp

```lua
IsPrivateIp(uint32)
```

**Parameters:**

- `uint32` (integer)

**Returns:**

- `boolean`: # `true` if IP address is part of a private network (10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16).

### IsPublicIp

```lua
IsPublicIp(uint32)
```

Note: we intentionally regard TEST-NET IPs as public.

**Parameters:**

- `uint32` (integer)

**Returns:**

- `boolean`: # `true` if IP address is not a private network (`10.0.0.0/8`, `172.16.0.0/12`, `192.168.0.0/16`) and is not localhost (`127.0.0.0/8`).

### IsReasonablePath

```lua
IsReasonablePath(str)
```

**Parameters:**

- `str` (string)

**Returns:**

- `boolean`: # `true` if path doesn't contain "." or ".." segments See `isreasonablepath.c`

### IsTrustedIp

```lua
IsTrustedIp(ip)
```

Returns `true` if IP address is trustworthy.
If the `ProgramTrustedIp()` function has NOT been called then redbean
will consider the networks 127.0.0.0/8, 10.0.0.0/8, 172.16.0.0/12,
and 192.168.0.0/16 to be trustworthy too. If `ProgramTrustedIp()` HAS
been called at some point earlier in your redbean's lifecycle, then
it'll trust the IPs and network subnets you specify instead.
The network interface addresses used by the host machine are always
considered trustworthy, e.g. 127.0.0.1. This may change soon, if we
decide to export a `GetHostIps()` API which queries your NIC devices.

**Parameters:**

- `ip` (integer)

**Returns:**

- `boolean`

### LaunchBrowser

```lua
LaunchBrowser(path)
```

Launches web browser on local machine with URL to this redbean server. This function may be called from your `/.init.lua`.

**Parameters:**

- `path` (string?)

### Lemur64

```lua
Lemur64()
```

This linear congruential generator passes practrand and bigcrush.

**Returns:**

- `integer`: # fastest pseudorandom non-cryptographic random number.

### LoadAsset

```lua
LoadAsset(path)
```

The asset may be sourced from either the zip (decompressed) or the local
filesystem if the `-D` flag was used. If slurping large file into memory is a
concern, then consider using `ServeAsset` which can serve directly off disk.

**Parameters:**

- `path` (string)

**Returns:**

- `string`: asset contents of file as string.

### Log

```lua
Log(level, message)
```

Emits message string to log, if level is less than or equal to `GetLogLevel`.
If redbean is running in interactive mode, then this will log to the console.
If redbean is running as a daemon or the `-L LOGFILE` flag is passed, then this
will log to the file. Reasonable values for level are `kLogDebug` >
`kLogVerbose` > `kLogInfo` > `kLogWarn` > `kLogError` > `kLogFatal`. The
logger emits timestamps in the local timezone with microsecond precision. If
log entries are emitted more frequently than once per second, then the log
entry will display a delta timestamp, showing how much time has elapsed since
the previous log entry. This behavior is useful for quickly measuring how long
various portions of your code take to execute.

**Parameters:**

- `level` (integer)
- `message` (string)

### Md5

```lua
Md5(str)
```

Computes MD5 checksum, returning 16 bytes of binary.

**Parameters:**

- `str` (string)

**Returns:**

- `string`: checksum

### MeasureEntropy

```lua
MeasureEntropy(data)
```

This gives you an idea of the density of information. Cryptographic random
should be in the ballpark of `7.9` whereas plaintext will be more like `4.5`.

**Parameters:**

- `data` (string)

**Returns:**

- `number`: # Shannon entropy of `data`.

### OnClientConnection

```lua
OnClientConnection(ip, port, serverip, serverport)
```

Hooks client connection creation.
If this function is defined it'll be called from the main process
each time redbean accepts a new client connection.

**Parameters:**

- `ip` (uint32)
- `port` (uint16)
- `serverip` (uint32)
- `serverport` (uint16)

**Returns:**

- `boolean`: # If it returns `true`, redbean will close the connection without calling `fork`.

### OnError

```lua
OnError(status, message)
```

Hooks catch errors
If this functiopn is defined in the global scope by your `/.init.lua`
then any errors occuring in the OnHttpRequest() hook will be catched.
You'll be able then to do whatever you need with the error status and
error message.

**Parameters:**

- `status` (uint16)
- `message` (string)

### OnHttpRequest

```lua
OnHttpRequest()
```

Hooks HTTP message handling.
If this function is defined in the global scope by your `/.init.lua`
then redbean will call it at the earliest possible moment to
hand over control for all messages (with the exception of `OPTIONS--*`
See functions like `Route` which asks redbean to do its default
thing from the handler.

### OnLogLatency

```lua
OnLogLatency(reqtimeus, contimeus)
```

Hook latency logging.
If this function is defined it'll be called from the main process
each time redbean completes handling of a request, but before the
response is sent. The handler received the time (in µs) since the
request handling and connection handling started.

**Parameters:**

- `reqtimeus` (integer)
- `contimeus` (integer)

### OnProcessCreate

```lua
OnProcessCreate(pid, ip, port, serverip, serverport)
```

Hooks process creation.
If this function is defined it'll be called from the main process
each time redbean forks a connection handler worker process. The
ip/port of the remote client is provided, along with the ip/port
of the listening interface that accepted the connection. This may
be used to create a server activity dashboard, in which case the
data provider handler should set `SetHeader('Connection','Close')`.
This won't be called in uniprocess mode.

**Parameters:**

- `pid` (integer)
- `ip` (uint32)
- `port` (uint16)
- `serverip` (uint32)
- `serverport` (uint16)

### OnProcessDestroy

```lua
OnProcessDestroy(pid)
```

If this function is defined it'll be called from the main process
each time redbean reaps a child connection process using `wait4()`.
This won't be called in uniprocess mode.

**Parameters:**

- `pid` (integer)

### OnServerHeartbeat

```lua
OnServerHeartbeat()
```

If this function is defined it'll be called from the main process
on each server heartbeat. The heartbeat interval is configurable
with `ProgramHeartbeatInterval`.

### OnServerListen

```lua
OnServerListen(socketdescriptor, serverip, serverport)
```

If this function is defined it'll be called from the main process
before redbean starts listening on a port. This hook can be used
to modify socket configuration to set `SO_REUSEPORT`, for example.

**Parameters:**

- `socketdescriptor` (integer)
- `serverip` (uint32)
- `serverport` (uint16)

**Returns:**

- `boolean`: ignore If `true`, redbean will not listen to that ip/port.

### OnServerReload

```lua
OnServerReload(reindex)
```

If this function is defined it'll be called from the main process
on each server reload triggered by SIGHUP (for daemonized) and
SIGUSR1 (for all) redbean instances. `reindex` indicates if redbean
assets have been re-indexed following the signal.

**Parameters:**

- `reindex` (boolean)

### OnServerStart

```lua
OnServerStart()
```

If this function is defined it'll be called from the main process
right before the main event loop starts.

### OnServerStop

```lua
OnServerStop()
```

If this function is defined it'll be called from the main process
after all the connection processes have been reaped and `exit()` is
ready to be called.

### OnWorkerStart

```lua
OnWorkerStart()
```

If this function is defined it'll be called from the child worker
process after it's been forked and before messages are handled.
This won't be called in uniprocess mode.

### OnWorkerStop

```lua
OnWorkerStop()
```

If this function is defined it'll be called from the child worker
process once `_exit()` is ready to be called. This won't be called
in uniprocess mode.

### ParseHttpDateTime

```lua
ParseHttpDateTime(rfc1123)
```

Converts RFC1123 string that looks like this: Mon, 29 Mar 2021 15:37:13 GMT to
a UNIX timestamp. See `parsehttpdatetime.c`.

**Parameters:**

- `rfc1123` (string)

**Returns:**

- `integer`: seconds

### ParseIp

```lua
ParseIp(ip)
```

Converts IPv4 address string to integer, e.g. "1.2.3.4" → 0x01020304, or
returns -1 for invalid inputs. See also `FormatIp` for the inverse operation.

**Parameters:**

- `ip` (string)

**Returns:**

- `integer`: ip

### ParseUrl

```lua
ParseUrl(url, flags)
```

Parses URL.
- `scheme` is a string, e.g. `"http"`
- `user` is the username string, or nil if absent
- `pass` is the password string, or nil if absent
- `host` is the hostname string, or nil if `url` was a path
- `port` is the port string, or nil if absent
- `path` is the path string, or nil if absent
- `params` is the URL paramaters, e.g. `/?a=b&c` would be
represented as the data structure `{{"a", "b"}, {"c"}, ...}`
- `fragment` is the stuff after the `#` character
- `kUrlPlus` to turn `+` into space
- `kUrlLatin1` to transcode ISO-8859-1 input into UTF-8
This parser is charset agnostic. Percent encoded bytes are
decoded for all fields. Returned values might contain things
like NUL characters, spaces, control codes, and non-canonical
encodings. Absent can be discerned from empty by checking if
the pointer is set.
There's no failure condition for this routine. This is a
permissive parser. This doesn't normalize path segments like
`.` or `..` so use IsAcceptablePath() to check for those. No
restrictions are imposed beyond that which is strictly
necessary for parsing. All the data that is provided will be
consumed to the one of the fields. Strict conformance is
enforced on some fields more than others, like scheme, since
it's the most non-deterministically defined field of them all.
Please note this is a URL parser, not a URI parser. Which
means we support everything the URI spec says we should do
except for the things we won't do, like tokenizing path
segments into an array and then nesting another array beneath
each of those for storing semicolon parameters. So this parser
won't make SIP easy. What it can do is parse HTTP URLs and most
URIs like data:opaque, better in fact than most things which
claim to be URI parsers.

**Parameters:**

- `url` (string)
- `flags` (integer?): may have:

**Returns:**

- `Url`: url An object containing the following fields is returned:

### Popcnt

```lua
Popcnt(x)
```

Returns number of bits set in integer.

**Parameters:**

- `x` (integer)

**Returns:**

- `integer`

### ProgramAddr

```lua
ProgramAddr(ip)
```

Configures the address on which to listen. This can be called multiple times
to set more than one address. If an integer is provided then it should be a
word-encoded IPv4 address, such as the ones returned by `ResolveIp()`. If a
string is provided, it will first be passed to ParseIp() to see if it's an
IPv4 address. If it isn't, then a HOSTS.TXT lookup is performed, with fallback
to the system-configured DNS resolution service. Please note that in MODE=tiny
the HOSTS.TXT and DNS resolution isn't included, and therefore an IP must be
provided.

**Parameters:**

- `ip` (integer)

### ProgramBrand

```lua
ProgramBrand(str)
```

Changes HTTP Server header, as well as the `<h1>` title on the `/` listing page.
The brand string needs to be a UTF-8 value that's encodable as ISO-8859-1.
If the brand is changed to something other than redbean, then the promotional
links will be removed from the listing page too. This function should only be
called from `/.init.lua`.

**Parameters:**

- `str` (string)

### ProgramCache

```lua
ProgramCache(seconds, directive)
```

Configures Cache-Control and Expires header generation for static asset serving.
A negative value will disable the headers. Zero means don't cache. Greater than
zero asks public proxies and browsers to cache for a given number of seconds.
The directive value is added to the Cache-Control header when specified (with
"must-revalidate" provided by default) and can be set to an empty  string to
remove the default value.
This should only be called from `/.init.lua`.

**Parameters:**

- `seconds` (integer)
- `directive` (string?)

### ProgramCertificate

```lua
ProgramCertificate(pem)
```

This function is the same as the -C flag if called from `.init.lua`, e.g.
`ProgramCertificate(LoadAsset("/.sign.crt"))` for zip loading or
`ProgramCertificate(Slurp("/etc/letsencrypt.lol/fullchain.pem"))` for local
file system only. This function is not available in unsecure mode.

**Parameters:**

- `pem` (string)

### ProgramDirectory

```lua
ProgramDirectory(str)
```

Same as the `-D` flag if called from `.init.lua` for overlaying local file
system directories. This may be called multiple times. The first directory
programmed is preferred. These currently do not show up in the index page listing.

**Parameters:**

- `str` (string)

### ProgramGid

```lua
ProgramGid(int)
```

Same as the `-G` flag if called from `.init.lua` for `setgid()`

**Parameters:**

- `int` (integer)

### ProgramHeader

```lua
ProgramHeader(name, value)
```

Appends HTTP header to the header buffer for all responses (whereas `SetHeader`
only appends a header to the current response buffer). name is case-insensitive
and restricted to non-space ASCII. value is a UTF-8 string that must be
encodable as ISO-8859-1. Leading and trailing whitespace is trimmed
automatically. Overlong characters are canonicalized. C0 and C1 control codes
are forbidden, with the exception of tab. The header buffer is independent of
the payload buffer. This function disallows the setting of certain headers
such as Content-Range and Date, which are abstracted by the transport layer.

**Parameters:**

- `name` (string)
- `value` (string)

### ProgramHeartbeatInterval

```lua
ProgramHeartbeatInterval(milliseconds)
```

Sets the heartbeat interval (in milliseconds). 5000ms is the default and 100ms is the minimum.

**Parameters:**

- `milliseconds` (integer): Negative values `(<0)` sets the interval in seconds.

### ProgramLogBodies

```lua
ProgramLogBodies(bool)
```

Same as the `-b` flag if called from `.init.lua` for logging message bodies as
part of `POST` / `PUT` / etc. requests.

**Parameters:**

- `bool` (boolean)

### ProgramLogMessages

```lua
ProgramLogMessages(bool)
```

Same as the `-m` flag if called from `.init.lua` for logging message headers only.

**Parameters:**

- `bool` (boolean)

### ProgramLogPath

```lua
ProgramLogPath(str)
```

Same as the `-L` flag if called from `.init.lua` for setting the log file path
on the local file system. It's created if it doesn't exist. This is called
before de-escalating the user / group id. The file is opened in append only
mode. If the disk runs out of space then redbean will truncate the log file if
has access to change the log file after daemonizing.

**Parameters:**

- `str` (string)

### ProgramMaxPayloadSize

```lua
ProgramMaxPayloadSize(int)
```

Sets the maximum HTTP message payload size in bytes. The
default is very conservatively set to 65536 so this is
something many people will want to increase. This limit is
enforced at the transport layer, before any Lua code is
called, because right now redbean stores and forwards
messages. (Use the UNIX API for raw socket streaming.) Setting
this to a very high value can be useful if you're less
concerned about rogue clients and would rather have your Lua
code be granted more control to bounce unreasonable messages.
If a value less than 1450 is supplied, it'll automatically be
increased to 1450, since that's the size of ethernet frames.
This function can only be called from `.init.lua`.

**Parameters:**

- `int` (integer)

### ProgramPidPath

```lua
ProgramPidPath(str)
```

Same as the `-P` flag if called from `.init.lua` for setting the pid file path
on the local file system. It's useful for reloading daemonized redbean using
`kill -HUP $(cat /var/run/redbean.pid)` or terminating redbean with
`kill $(cat /var/run/redbean.pid)` which will gracefully terminate all clients.
Sending the `TERM` signal twice will cause a forceful shutdown, which might
make someone with a slow internet connection who's downloading big files unhappy.

**Parameters:**

- `str` (string)

### ProgramPort

```lua
ProgramPort(uint16)
```

Hard-codes the port number on which to listen, which can be any number in the
range `1..65535`, or alternatively `0` to ask the operating system to choose a
port, which may be revealed later on by `GetServerAddr` or the `-z` flag to stdout.

**Parameters:**

- `uint16` (integer)

### ProgramPrivateKey

```lua
ProgramPrivateKey(pem)
```

This function is the same as the -K flag if called from .init.lua, e.g.
`ProgramPrivateKey(LoadAsset("/.sign.key"))` for zip loading or
`ProgramPrivateKey(Slurp("/etc/letsencrypt/privkey.pem"))` for local file
system only. This function is not available in unsecure mode.

**Parameters:**

- `pem` (string)

### ProgramRedirect

```lua
ProgramRedirect(code, src, location)
```

Configures fallback routing for paths which would otherwise return 404 Not
Found. If code is `0` then the path is rewritten internally as an accelerated
redirect. If code is `301`, `302`, `307`, or `308` then a redirect response
will be sent to the client. This should only be called from `/.init.lua`.

**Parameters:**

- `code` (integer)
- `src` (string)
- `location` (string)

### ProgramSslCiphersuite

```lua
ProgramSslCiphersuite(name)
```

This function may be called multiple times to specify the subset of available
ciphersuites you want to use in both the HTTPS server and the `Fetch()` client.
The default list, ordered by preference, is as follows:
- ECDHE-ECDSA-AES256-GCM-SHA384
- ECDHE-ECDSA-AES128-GCM-SHA256
- ECDHE-ECDSA-CHACHA20-POLY1305-SHA256
- ECDHE-PSK-AES256-GCM-SHA384
- ECDHE-PSK-AES128-GCM-SHA256
- ECDHE-PSK-CHACHA20-POLY1305-SHA256
- ECDHE-RSA-AES256-GCM-SHA384
- ECDHE-RSA-AES128-GCM-SHA256
- ECDHE-RSA-CHACHA20-POLY1305-SHA256
- DHE-RSA-AES256-GCM-SHA384
- DHE-RSA-AES128-GCM-SHA256
- DHE-RSA-CHACHA20-POLY1305-SHA256
- ECDHE-ECDSA-AES128-CBC-SHA256
- ECDHE-RSA-AES256-CBC-SHA384
- ECDHE-RSA-AES128-CBC-SHA256
- DHE-RSA-AES256-CBC-SHA256
- DHE-RSA-AES128-CBC-SHA256
- ECDHE-PSK-AES256-CBC-SHA384
- ECDHE-PSK-AES128-CBC-SHA256
- ECDHE-ECDSA-AES256-CBC-SHA
- ECDHE-ECDSA-AES128-CBC-SHA
- ECDHE-RSA-AES256-CBC-SHA
- ECDHE-RSA-AES128-CBC-SHA
- DHE-RSA-AES256-CBC-SHA
- DHE-RSA-AES128-CBC-SHA
- ECDHE-PSK-AES256-CBC-SHA
- ECDHE-PSK-AES128-CBC-SHA
- RSA-AES256-GCM-SHA384
- RSA-AES128-GCM-SHA256
- RSA-AES256-CBC-SHA256
- RSA-AES128-CBC-SHA256
- RSA-AES256-CBC-SHA
- RSA-AES128-CBC-SHA
- PSK-AES256-GCM-SHA384
- PSK-AES128-GCM-SHA256
- PSK-CHACHA20-POLY1305-SHA256
- PSK-AES256-CBC-SHA384
- PSK-AES128-CBC-SHA256
- PSK-AES256-CBC-SHA
- PSK-AES128-CBC-SHA
- ECDHE-RSA-3DES-EDE-CBC-SHA
- DHE-RSA-3DES-EDE-CBC-SHA
- ECDHE-PSK-3DES-EDE-CBC-SHA
- RSA-3DES-EDE-CBC-SHA
- PSK-3DES-EDE-CBC-SHA
When redbean is run on an old (or low-power) CPU that doesn't have the AES-NI
instruction set (Westmere c. 2010) then the default ciphersuite is tuned
automatically to favor the ChaCha20 Poly1305 suites.
The names above are canonical to redbean. They were programmatically simplified
from the official IANA names. This function will accept the IANA names too. In
most cases it will accept the OpenSSL and GnuTLS naming convention as well.
This function is not available in unsecure mode.

**Parameters:**

- `name` (string)

### ProgramSslClientVerify

```lua
ProgramSslClientVerify(enabled)
```

Enables the verification of certificates supplied by the HTTP clients that
connect to your redbean. This has the same effect as the -j flag. Tuning this
option alone does not preclude the possibility of unsecured HTTP clients, which
can be disabled using ProgramSslRequired(). This function can only be called
from `.init.lua`. This function is not available in unsecure mode.

**Parameters:**

- `enabled` (boolean)

### ProgramSslFetchVerify

```lua
ProgramSslFetchVerify(enabled)
```

May be used to disable the verification of certificates
for remote hosts when using the Fetch() API. This function is
not available in unsecure mode.

**Parameters:**

- `enabled` (boolean)

### ProgramSslPresharedKey

```lua
ProgramSslPresharedKey(key, identity)
```

This function can be used to enable the PSK ciphersuites which simplify SSL
and enhance its performance in controlled environments. key may contain 1..32
bytes of random binary data and identity is usually a short plaintext string.
The first time this function is called, the preshared key will be added to
both the client and the server SSL configs. If it's called multiple times,
then the remaining keys will be added to the server, which is useful if you
want to assign separate keys to each client, each of which needs a separate
identity too. If this function is called multiple times with the same identity
string, then the latter call will overwrite the prior. If a preshared key is
supplied and no certificates or key-signing-keys are programmed, then redbean
won't bother auto-generating any serving certificates and will instead use
only PSK ciphersuites. This function is not available in unsecure mode.

**Parameters:**

- `key` (string)
- `identity` (string)

### ProgramSslRequired

```lua
ProgramSslRequired(mandatory)
```

Enables the blocking of HTTP so that all inbound clients and must use the TLS
transport layer. This has the same effect as the -J flag. `Fetch()` is still
allowed to make outbound HTTP requests. This function can only be called from
`.init.lua`. This function is not available in unsecure mode.

**Parameters:**

- `mandatory` (string)

### ProgramSslTicketLifetime

```lua
ProgramSslTicketLifetime(seconds)
```

Defaults to `86400` (24 hours). This may be set to `≤0` to disable SSL tickets.
It's a good idea to use these since it increases handshake performance 10x and
eliminates a network round trip. This function is not available in unsecure mode.

**Parameters:**

- `seconds` (integer)

### ProgramTimeout

```lua
ProgramTimeout(milliseconds)
```

Default timeout is 60000ms. Minimal value of timeout is 10(ms).
This should only be called from `/.init.lua`.

**Parameters:**

- `milliseconds` (integer): Negative values `(<0)` sets the interval in seconds.

### ProgramTokenBucket

```lua
ProgramTokenBucket(replenish, cidr, reject, ignore, ban)
```

Enables DDOS protection.
Imagine you have 2**32 buckets, one for each IP address. Each bucket
can hold about 127 tokens. Every second a background worker puts one
token in each bucket. When a TCP client socket is opened, it takes a
token from its bucket, and then proceeds. If the bucket holds only a
third of its original tokens, then redbean sends them a 429 warning.
If the client ignores this warning and keeps sending requests, until
there's no tokens left, then the banhammer finally comes down.
function OnServerStart()
ProgramTokenBucket()
ProgramTrustedIp(ParseIp('x.x.x.x'), 32)
assert(unix.setrlimit(unix.RLIMIT_NPROC, 1000, 1000))
end
This model of network rate limiting generously lets people "burst" a
tiny bit. For example someone might get a strong craving for content
and smash the reload button in Chrome 64 times in a fow seconds. But
since the client only get 1 new token per second, they'd better cool
their heels for a few minutes after doing that. This amount of burst
can be altered by choosing the `reject` / `ignore` / `ban` threshold
arguments. For example, if the `reject` parameter is set to 126 then
no bursting is allowed, which probably isn't a good idea.
redbean is programmed to acquire a token immediately after accept()
is called from the main server process, which is well before fork()
or read() or any Lua code happens. redbean then takes action, based
on the token count, which can be accept / reject / ignore / ban. If
redbean determines a ban is warrented, then 4-byte datagram is sent
to the unix domain socket `/var/run/blackhole.sock` which should be
operated using the blackholed program we distribute separately.
The trick redbean uses on Linux for example is insert rules in your
raw prerouting table. redbean is very fast at the application layer
so the biggest issue we've encountered in production is are kernels
themselves, and programming the raw prerouting table dynamically is
how we solved that.
`replenish` is the number of times per second a token should be
added to each bucket. The default value is 1 which means one token
is granted per second to all buckets. The minimum value is 1/3600
which means once per hour. The maximum value for this setting is
1e6, which means once every microsecond.
`cidr` is the specificity of judgement.  Since creating 2^32 buckets
would need 4GB of RAM, redbean defaults this value to 24 which means
filtering applies to class c network blocks (i.e. x.x.x.*), and your
token buckets only take up 2^24 bytes of RAM (16MB). This can be set
to any number on the inclusive interval [8,32], where having a lower
number means you use less ram/cpu, but splash damage applies more to
your clients; whereas higher numbers means more ram/cpu usage, while
ensuring rate limiting only applies to specific compromised actors.
`reject` is the token count or treshold at which redbean should send
429 Too Many Request warnings to the client. Permitted values can be
anywhere between -1 and 126 inclusively. The default value is 30 and
-1 means disable to disable (assuming AcquireToken() will be used).
`ignore` is the token count or treshold, at which redbean should try
simply ignoring clients and close the connection without logging any
kind of warning, and without sending any response. The default value
for this setting is `MIN(reject / 2, 15)`. This must be less than or
equal to the `reject` setting. Allowed values are [-1,126] where you
can use -1 as a means of disabling `ignore`.
`ban` is the token count at which redbean should report IP addresses
to the blackhole daemon via a unix-domain socket datagram so they'll
get banned in the kernel routing tables. redbean's default value for
this setting is `MIN(ignore / 10, 1)`. Permitted values are [-1,126]
where -1 may be used as a means of disabling the `ban` feature.
This function throws an exception if the constraints described above
are not the case. Warnings are logged should redbean fail to connect
to the blackhole daemon, assuming it hasn't been disabled. It's safe
to use load balancing tools when banning is enabled, since you can't
accidentally ban your own network interface addresses, loopback ips,
or ProgramTrustedIp() addresses where these rate limits don't apply.
It's assumed will be called from the .init.lua global scope although
it could be used in interpreter mode, or from a forked child process
in which case the only processes that'll have ability to use it will
be that same process, and any descendent processes. This function is
only able to be called once.
This feature is not available in unsecure mode.

**Parameters:**

- `replenish` (number?)
- `cidr` (integer?)
- `reject` (integer?)
- `ignore` (integer?)
- `ban` (integer?)

### ProgramTrustedIp

```lua
ProgramTrustedIp(ip, cidr)
```

Trusts an IP address or network
This function may be used to configure the `IsTrustedIp()` function
which is how redbean determines if a client is allowed to send us
headers like X-Forwarded-For (cf `GetRemoteAddr` vs. `GetClientAddr`)
without them being ignored. Trusted IPs is also how redbean turns
off token bucket rate limiting selectively, so be careful. Here's
an example of how you could trust all of Cloudflare's IPs:
ProgramTrustedIp(ParseIp("103.21.244.0"), 22);
ProgramTrustedIp(ParseIp("103.22.200.0"), 22);
ProgramTrustedIp(ParseIp("103.31.4.0"), 22);
ProgramTrustedIp(ParseIp("104.16.0.0"), 13);
ProgramTrustedIp(ParseIp("104.24.0.0"), 14);
ProgramTrustedIp(ParseIp("108.162.192.0"), 18);
ProgramTrustedIp(ParseIp("131.0.72.0"), 22);
ProgramTrustedIp(ParseIp("141.101.64.0"), 18);
ProgramTrustedIp(ParseIp("162.158.0.0"), 15);
ProgramTrustedIp(ParseIp("172.64.0.0"), 13);
ProgramTrustedIp(ParseIp("173.245.48.0"), 20);
ProgramTrustedIp(ParseIp("188.114.96.0"), 20);
ProgramTrustedIp(ParseIp("190.93.240.0"), 20);
ProgramTrustedIp(ParseIp("197.234.240.0"), 22);
ProgramTrustedIp(ParseIp("198.41.128.0"), 17);
Although you might want consider trusting redbean's open source
freedom embracing solution to DDOS protection instead!

**Parameters:**

- `ip` (integer)
- `cidr` (integer?)

### ProgramUid

```lua
ProgramUid(int)
```

Same as the `-U` flag if called from `.init.lua` for `setuid()`

### ProgramUniprocess

```lua
ProgramUniprocess(bool)
```

Same as the `-u` flag if called from `.init.lua`. Can be used to configure the
uniprocess mode. The current value is returned.

**Parameters:**

- `bool` (boolean?)

**Returns:**

- `boolean`

### Rand64

```lua
Rand64()
```

This linear congruential generator passes practrand and bigcrush. This
generator is safe across `fork()`, threads, and signal handlers.

**Returns:**

- `integer`: # nondeterministic pseudorandom non-cryptographic number.

### Rdrand

```lua
Rdrand()
```

**Returns:**

- `integer`: # 64-bit hardware random integer from RDRND instruction, with automatic fallback to `getrandom()` if not available.

### Rdseed

```lua
Rdseed()
```

**Returns:**

- `integer`: # 64-bit hardware random integer from `RDSEED` instruction, with automatic fallback to `RDRND` and `getrandom()` if not available.

### Rdtsc

```lua
Rdtsc()
```

Returns CPU timestamp counter.

**Returns:**

- `integer`

### ResolveIp

```lua
ResolveIp(hostname)
```

Gets IP address associated with hostname.
This function first checks if hostname is already an IP address, in which case
it returns the result of `ParseIp`. Otherwise, it checks HOSTS.TXT on the local
system and returns the first IPv4 address associated with hostname. If no such
entry is found, a DNS lookup is performed using the system configured (e.g.
`/etc/resolv.conf`) DNS resolution service. If the service returns multiple IN
A records then only the first one is returned.
The returned address is word-encoded in host endian order. For example,
1.2.3.4 is encoded as 0x01020304. The `FormatIp` function may be used to turn
this value back into a string.
If no IP address could be found, then `nil` is returned alongside a string of
unspecified format describing the error. Calls to this function may be wrapped
in `assert()` if an exception is desired.

**Parameters:**

- `hostname` (string)

**Returns:**

- `uint32`: ip uint32

### Route

```lua
Route(host, path)
```

Instructs redbean to follow the normal HTTP serving path. This function is
useful when writing an OnHttpRequest handler, since that overrides the
serving path entirely. So if the handler decides it doesn't want to do
anything, it can simply call this function, to hand over control back to the
redbean core. By default, the host and path arguments are supplied from the
resolved `GetUrl` value. This handler always resolves, since it will generate
a 404 Not Found response if redbean couldn't find an appropriate endpoint.

**Parameters:**

- `host` (string?)
- `path` (string?)

### RouteHost

```lua
RouteHost(host, path)
```

This is the same as `Route`, except it only implements the subset of request
routing needed for serving virtual-hosted assets, where redbean tries to prefix
the path with the hostname when looking up a file. This function returns `true`
if the request was resolved. If it was resolved, then your `OnHttpRequest`
request handler can still set additional headers.

**Parameters:**

- `host` (string?)
- `path` (string?)

**Returns:**

- `boolean`

### RoutePath

```lua
RoutePath(path)
```

This is the same as `Route`, except it only implements the subset of request
routing needed for serving assets. This function returns `true` if the
request was resolved. If it was resolved, then your `OnHttpRequest` request
handler can still set additional headers.

**Parameters:**

- `path` (string?)

**Returns:**

- `boolean`

### ServeAsset

```lua
ServeAsset(path)
```

Instructs redbean to serve static asset at path. This function causes what
would normally happen outside a dynamic handler to happen. The asset can be
sourced from either the zip or local filesystem if `-D` is used. This function
is mutually exclusive with `SetStatus` and `ServeError`.

**Parameters:**

- `path` (string)

### ServeError

```lua
ServeError(code, reason)
```

Instructs redbean to serve a boilerplate error page. This takes care of logging
the error, setting the reason phrase, and adding a payload. This function is
mutually exclusive with `SetStatus` and `ServeAsset`.

**Parameters:**

- `code` (integer)
- `reason` (string?)

### ServeRedirect

```lua
ServeRedirect(code, location)
```

Instructs redbean to return the specified redirect code along with
the Location header set. This function is mutually exclusive with
`SetStatus` and other `Serve*` functions.

**Parameters:**

- `code` (integer)
- `location` (string)

### SetCookie

```lua
SetCookie(name, value, options)
```

Appends Set-Cookie HTTP header to the response header buffer.
Several Set-Cookie headers can be added to the same response.
`__Host-` and `__Secure-` prefixes are supported and may set or
overwrite some of the options (for example, specifying `__Host-`
prefix sets the Secure option to `true`, sets the path to `"/"`, and
removes the Domain option). The following options can be used (their
lowercase equivalents are supported as well):
- `Expires` sets the maximum lifetime of the cookie as an HTTP-date timestamp. Can be specified as a Date in the RFC1123 (string) format or as a UNIX timestamp (number of seconds).
- `MaxAge` sets number of seconds until the cookie expires. A zero or negative number will expire the cookie immediately. If both Expires and MaxAge are set, MaxAge has precedence.
- `Domain` sets the host to which the cookie will be sent.
- `Path` sets the path that must be present in the request URL, or the client will not send the Cookie header.
- `Secure` (boolean) requests the cookie to be only send to the server when a request is made with the https: scheme.
- `HttpOnly` (boolean) forbids JavaScript from accessing the cookie.
- `SameSite` (Strict, Lax, or None) controls whether a cookie is sent with cross-origin requests, providing some protection against cross-site request forgery attacks.

**Parameters:**

- `name` (string)
- `value` (string)
- `options` ({): Expires: string|integer?, MaxAge: integer?, Domain: string?, Path: string?, Secure: boolean?, HttpOnly: boolean?, SameSite: "Strict"|"Lax"|"None"? }?

### SetHeader

```lua
SetHeader(name, value)
```

Appends HTTP header to response header buffer.
Leading and trailing whitespace is trimmed automatically. Overlong
characters are canonicalized. C0 and C1 control codes are forbidden,
with the exception of tab. This function automatically calls
`SetStatus(200, "OK")` if a status has not yet been set. As
`SetStatus` and `Serve*` functions reset the response, `SetHeader`
needs to be called after `SetStatus` and `Serve*` functions are
called. The header buffer is independent of the payload buffer.
Neither is written to the wire until the Lua Server Page has
finished executing. This function disallows the setting of certain
headers such as and Content-Range which are abstracted by the
transport layer. In such cases, consider calling `ServeAsset`.

**Parameters:**

- `name` (string): is case-insensitive and restricted to non-space ASCII
- `value` (string): is a UTF-8 string that must be encodable as ISO-8859-1.

### SetLogLevel

```lua
SetLogLevel(level)
```

Sets logger verbosity. Reasonable values for level are `kLogDebug` >
`kLogVerbose` > `kLogInfo` > `kLogWarn` > `kLogError` > `kLogFatal`.

**Parameters:**

- `level` (integer)

### SetStatus

```lua
SetStatus(code, reason)
```

Starts an HTTP response, specifying the parameters on its first line.
This method will reset the response and is therefore mutually
exclusive with `ServeAsset` and other `Serve*` functions. If a
status setting function isn't called, then the default behavior is
to send `200 OK`.

**Parameters:**

- `code` (integer)
- `reason` (string) *(optional)*: is optional since redbean can fill in the appropriate text for well-known magic numbers, e.g. `200`, `404`, etc.

### Sha1

```lua
Sha1(str)
```

Computes SHA1 checksum, returning 20 bytes of binary.

**Parameters:**

- `str` (string)

**Returns:**

- `string`: checksum

### Sha224

```lua
Sha224(str)
```

Computes SHA224 checksum, returning 28 bytes of binary.

**Parameters:**

- `str` (string)

**Returns:**

- `string`: checksum

### Sha256

```lua
Sha256(str)
```

Computes SHA256 checksum, returning 32 bytes of binary.

**Parameters:**

- `str` (string)

**Returns:**

- `string`: checksum

### Sha384

```lua
Sha384(str)
```

Computes SHA384 checksum, returning 48 bytes of binary.

**Parameters:**

- `str` (string)

**Returns:**

- `string`: checksum

### Sha512

```lua
Sha512(str)
```

Computes SHA512 checksum, returning 64 bytes of binary.

**Parameters:**

- `str` (string)

**Returns:**

- `string`: checksum

### Sleep

```lua
Sleep(seconds)
```

Sleeps the specified number of seconds (can be fractional).
The smallest interval is a microsecond.

**Parameters:**

- `seconds` (number)

### Slurp

```lua
Slurp(filename, i, j)
```

Reads all data from file the easy way.
This function reads file data from local file system. Zip file assets can be
accessed using the `/zip/...` prefix.
`i` and `j` may be used to slice a substring in filename. These parameters are
1-indexed and behave consistently with Lua's `string.sub()` API. For example:
assert(Barf('x.txt', 'abc123'))
assert(assert(Slurp('x.txt', 2, 3)) == 'bc')
This function is uninterruptible so `unix.EINTR` errors will be ignored. This
should only be a concern if you've installed signal handlers. Use the UNIX API
if you need to react to it.

**Parameters:**

- `filename` (string)
- `i` (integer?)
- `j` (integer?)

**Returns:**

- `string`: data

### StoreAsset

```lua
StoreAsset(path, data, mode)
```

Stores asset to executable's ZIP central directory. This currently happens in
an append-only fashion and is still largely in the proof-of-concept stages.
Currently only supported on Linux, XNU, and FreeBSD. In order to use this
feature, the `-*` flag must be passed.

**Parameters:**

- `path` (string)
- `data` (string)
- `mode` (integer) *(optional)*

### Underlong

```lua
Underlong(str)
```

Canonicalizes overlong encodings.

**Parameters:**

- `str` (string)

**Returns:**

- `string`

### UuidV4

```lua
UuidV4()
```

Generate a uuid_v4
@return string

### UuidV7

```lua
UuidV7()
```

Generate a uuid_v7
@return string

### VisualizeControlCodes

```lua
VisualizeControlCodes(str)
```

Replaces C0 control codes and trojan source characters with descriptive
UNICODE pictorial representation. This function also canonicalizes overlong
encodings. C1 control codes are replaced with a JavaScript-like escape sequence.

**Parameters:**

- `str` (string)

**Returns:**

- `string`

### Write

```lua
Write(data)
```

represented as the data structure `{{"a", "b"}, {"c"}, ...}`
Appends data to HTTP response payload buffer.
This is buffered independently of headers.

**Parameters:**

- `data` (string)

### arg

```lua
arg
```

Array of command line arguments, excluding those parsed by
getopt() in the C code, which stops parsing at the first
non-hyphenated arg. In some cases you can use the magic --
argument to delimit C from Lua arguments.
For example, if you launch your redbean as follows:
redbean -v arg1 arg2
Then your `/.init.lua` file will have the `arg` array like:
arg[-1] = '/usr/bin/redbean'
arg[ 0] = '/zip/.init.lua'
arg[ 1] = 'arg1'
arg[ 2] = 'arg2'
If you launch redbean in interpreter mode (rather than web
server) mode, then an invocation like this:
./redbean -i script.lua arg1 arg2
Would have an `arg` array like this:
arg[-1] = './redbean'
arg[ 0] = 'script.lua'
arg[ 1] = 'arg1'
arg[ 2] = 'arg2'

### argon2

```lua
argon2
```

This module implements a password hashing algorithm based on blake2b that won
the Password Hashing Competition.
It can be used to securely store user passwords in your SQLite database, in a
way that destroys the password, but can be verified by regenerating the hash
again the next time the user logs in. Destroying the password is important,
since if your database is compromised, the bad guys won't be able to use
rainbow tables to recover the plain text of the passwords.
Argon2 achieves this security by being expensive to compute. Care should be
taken in choosing parameters, since an HTTP endpoint that uses Argon2 can just
as easily become a denial of service vector. For example, you may want to
consider throttling your login endpoint.

### bin

```lua
bin(int)
```

Formats string as binary integer literal string. If the provided value is zero,
the result will be `"0"`. Otherwise the resulting value will be the
"0b"-prefixed binary str. The result is currently modulo 2^64. Negative numbers
are converted to unsigned.

**Parameters:**

- `int` (integer)

**Returns:**

- `string`

### finger

```lua
finger
```

This is an experimental module that, like the maxmind module, gives you insight
into what kind of device is connecting to your redbean. This module can help
you protect your redbean because it provides tools for identifying clients that
misrepresent themselves. For example the User-Agent header might report itself
as a Windows computer when the SYN packet says it's a Linux computer.
function OnServerListen(fd, ip, port)
unix.setsockopt(fd, unix.SOL_TCP, unix.TCP_SAVE_SYN, true)
return false
end
function OnClientConnection(ip, port, serverip, serverport)
fd = GetClientFd()
syn = unix.getsockopt(fd, unix.SOL_TCP, unix.TCP_SAVED_SYN)
end
function OnHttpRequest()
Log(kLogInfo, "client is running %s and reports %s" % {
finger.GetSynFingerOs(finger.FingerSyn(syn)),
GetHeader('User-Agent')})
Route()
end

### hex

```lua
hex(int)
```

Formats string as hexadecimal integer literal string. If the provided value is
zero, the result will be `"0"`. Otherwise the resulting value will be the
"0x"-prefixed hex string. The result is currently modulo 2^64. Negative numbers
are converted to unsigned.

**Parameters:**

- `int` (integer)

**Returns:**

- `string`

### kLogDebug

```lua
kLogDebug
```

The error number is always different for different platforms. On
UNIX systems, error numbers occupy the range [1,127] in practice.
The System V ABI reserves numbers as high as 4095. On Windows NT,
error numbers can go up to 65535.
On UNIX systems this is always 0. On Windows NT this will normally
be the same as `errno(). Because Windows defines so many error codes,
there's oftentimes a multimapping between its error codes and System
Five. In those cases, this value reflects the `GetLastError()` result
at the time the error occurred.
For example, this might return `"EINTR"`.
For example, this might return `"read"` if `read()` was what failed.
For example, this might return `"Interrupted system call"`.
Different information components are delimited by slash.
For example, this might return `"EINTR/4/Interrupted system call"`.
On Windows NT this will include additional information about the
Windows error (including FormatMessage() output) if the WIN32 error
differs from the System Five error code.

### kLogFatal

```lua
kLogFatal
```

Logging anything at this level will result in a backtrace and process exit.

### lsqlite3

```lua
lsqlite3
```

Please refer to the LuaSQLite3 Documentation.
For example, you could put the following in your `/.init.lua` file:
lsqlite3 = require "lsqlite3"
db = lsqlite3.open_memory()
db:exec[[
CREATE TABLE test (
id INTEGER PRIMARY KEY,
content TEXT
);
INSERT INTO test (content) VALUES ('Hello World');
INSERT INTO test (content) VALUES ('Hello Lua');
INSERT INTO test (content) VALUES ('Hello Sqlite3');
]]
Then, your Lua server pages or OnHttpRequest handler may perform SQL
queries by accessing the db global. The performance is good too, at about
400k qps.
for row in db:nrows("SELECT * FROM test") do
Write(row.id.." "..row.content.."<br>")
end
redbean supports a subset of what's defined in the upstream LuaSQLite3
project. Most of the unsupported APIs relate to pointers and database
notification hooks.

### maxmind

```lua
maxmind
```

### MaxMind
This module may be used to get city/country/asn/etc from IPs, e.g.
-- .init.lua
maxmind = require 'maxmind'
asndb = maxmind.open('/usr/local/share/maxmind/GeoLite2-ASN.mmdb')
-- request handler
as = asndb:lookup(GetRemoteAddr())
if as then
asnum = as:get('autonomous_system_number')
asorg = as:get('autonomous_system_organization')
Write(EscapeHtml(asnum))
Write(' ')
Write(EscapeHtml(asorg))
end
The database file is distributed by MaxMind. You need to sign up on their
website to get a free copy. The database has a generalized structure. For a
concrete example of how this module may be used, please see `maxmind.lua`
in `redbean-demo`.

### oct

```lua
oct(int)
```

Formats string as octal integer literal string. If the provided value is zero,
the result will be `"0"`. Otherwise the resulting value will be the
zero-prefixed octal string. The result is currently modulo 2^64. Negative
numbers are converted to unsigned.

**Parameters:**

- `int` (integer)

**Returns:**

- `string`

### path

```lua
path
```

The path module may be used to manipulate unix paths.
Note that we use unix paths on Windows. For example, if you have a
path like `C:\foo\bar` then it should be `/c/foo/bar` with redbean.
It should also be noted the unix module is more permissive when
using Windows paths, where translation to win32 is very light.

### re

```lua
re
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

### unix

```lua
unix
```

This module exposes the low-level System Five system call interface.
This module works on all supported platforms, including Windows NT.
