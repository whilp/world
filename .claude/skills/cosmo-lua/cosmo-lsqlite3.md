# cosmo.lsqlite3

### lversion

```lua
lsqlite3.lversion()
```

**Returns:**

- `string`: version lsqlite3 library version information, in the form 'x.y[.z]'.

### open

```lua
lsqlite3.open(filename, flags)
```

Opens (or creates if it does not exist) an SQLite database with name filename
and returns its handle as userdata (the returned object should be used for all
further method calls in connection with this specific database, see Database
methods). Example:
myDB = lsqlite3.open('MyDatabase.sqlite3')  -- open
-- do some database calls...
myDB:close()  -- close
In case of an error, the function returns `nil`, an error code and an error message.
Since `0.9.4`, there is a second optional `flags` argument to `lsqlite3.open`.
See https://www.sqlite.org/c3ref/open.html for an explanation of these flags and options.
local db = lsqlite3.open('foo.db', lsqlite3.OPEN_READWRITE + lsqlite3.OPEN_CREATE + lsqlite3.OPEN_SHAREDCACHE)

**Parameters:**

- `filename` (string)
- `flags` (integer) *(optional)*: defaults to `lsqlite3.OPEN_READWRITE + lsqlite3.OPEN_CREATE`

**Returns:**

- `lsqlite3.Database`: db

### open_memory

```lua
lsqlite3.open_memory()
```

Opens an SQLite database in memory and returns its handle as userdata. In case
of an error, the function returns `nil`, an error code and an error message.
(In-memory databases are volatile as they are never stored on disk.)

**Returns:**

- `lsqlite3.Database`: db

### version

```lua
lsqlite3.version()
```

**Returns:**

- `string`: version SQLite version information, in the form 'x.y[.z[.p]]'.
