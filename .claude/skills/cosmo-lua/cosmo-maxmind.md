# cosmo.maxmind

### open

```lua
maxmind.open(filepath)
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

**Parameters:**

- `filepath` (string): the location of the MaxMind database

**Returns:**

- `maxmind.Db`: db
