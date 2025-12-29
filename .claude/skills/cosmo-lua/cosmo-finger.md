# cosmo.finger

### DescribeSyn

```lua
finger.DescribeSyn(syn_packet_bytes)
```

Describes IP+TCP SYN packet.
The layout looks as follows:
- `TTL:OPTIONS:WSIZE:MSS`
The `TTL`, `WSIZE`, and `MSS` fields are unsigned decimal fields.
The `OPTIONS` field communicates the ordering of the commonly used subset of
tcp options. The following character mappings are defined. TCP options not on
this list will be ignored.
- `E`: End of Option list
- `N`: No-Operation
- `M`: Maximum Segment Size
- `K`: Window Scale
- `O`: SACK Permitted
- `A`: SACK
- `e`: Echo (obsolete)
- `r`: Echo reply (obsolete)
- `T`: Timestamps
This function is nil/error propagating.

**Parameters:**

- `syn_packet_bytes` (string)

**Returns:**

- `string`: description

### FingerSyn

```lua
finger.FingerSyn(syn_packet_bytes)
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
Fingerprints IP+TCP SYN packet.
This returns a hash-like magic number that reflects the SYN packet structure,
e.g. ordering of options, maximum segment size, etc. We make no guarantees this
hashing algorithm won't change as we learn more about the optimal way to
- fingerprint, so be sure to save your syn packets too if you're using this
feature, in case they need to be rehashed in the future.
This function is nil/error propagating.

**Parameters:**

- `ip` (uint32): IPv4 address as uint32
- `syn_packet_bytes` (string)

**Returns:**

- `maxmind.Result?`: result
- `any`
- `integer`
- `integer`: synfinger uint32

### GetSynFingerOs

```lua
finger.GetSynFingerOs(synfinger)
```

Fingerprints IP+TCP SYN packet.
If synfinger is a known hard-coded magic number, then one of the following
strings may be returned:
- `"LINUX"`
- `"WINDOWS"`
- `"XNU"`
- `"NETBSD"`
- `"FREEBSD"`
- `"OPENBSD"`
If this function returns `nil`, then one thing you can do to help is file an
issue and share with us your SYN packet specimens. The way we prefer to receive
them is in `EncodeLua(syn_packet_bytes)` format along with details on the
operating system which you must know.

**Parameters:**

- `synfinger` (integer)

**Returns:**

- `"LINUX"|"WINDOWS"|"XNU"|"NETBSD"|"FREEBSD"|"OPENBSD"`: osname
