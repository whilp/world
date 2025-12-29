# cosmo.lsqlite3

### ABORT

```lua
lsqlite3.ABORT (constant)
```

The `lsqlite3.ABORT` result code indicates that an operation was aborted
prior to completion, usually be application request. See also:
`lsqlite3.INTERRUPT`.
If the callback function to `exec()` returns non-zero, then `exec()`
will return `lsqlite3.ABORT`.
If a ROLLBACK operation occurs on the same database connection as a
pending read or write, then the pending read or write may fail with an
`lsqlite3.ABORT` error.

### BUSY

```lua
lsqlite3.BUSY (constant)
```

The lsqlite3.BUSY result code indicates that the database file could not
be written (or in some cases read) because of concurrent activity by
some other database connection, usually a database connection in a
separate process.
For example, if process A is in the middle of a large write transaction
and at the same time process B attempts to start a new write
transaction, process B will get back an `lsqlite3.BUSY` result because
SQLite only supports one writer at a time. Process B will need to wait
for process A to finish its transaction before starting a new
transaction. The `db:busy_timeout()` and `db:busy_handler()` interfaces
are available to process B to help it deal with `lsqlite3.BUSY` errors.
An `lsqlite3.BUSY` error can occur at any point in a transaction: when
the transaction is first started, during any write or update operations,
or when the transaction commits. To avoid encountering `lsqlite3.BUSY`
errors in the middle of a transaction, the application can use
`BEGIN IMMEDIATE` instead of just `BEGIN` to start a transaction. The
`BEGIN IMMEDIATE` command might itself return `lsqlite3.BUSY`, but if it
succeeds, then SQLite guarantees that no subsequent operations on the same database through the next COMMIT will return `lsqlite3.BUSY`.
The `lsqlite3.BUSY` result code differs from `lsqlite3.LOCKED` in that
`lsqlite3.BUSY` indicates a conflict with a separate database
connection, probably in a separate process, whereas `lsqlite3.LOCKED`
indicates a conflict within the same database connection (or sometimes
a database connection with a shared cache).

### CANTOPEN

```lua
lsqlite3.CANTOPEN (constant)
```

The `lsqlite3.CANTOPEN` result code indicates that SQLite was unable to
open a file. The file in question might be a primary database file or
one of several temporary disk files.

### CONSTRAINT

```lua
lsqlite3.CONSTRAINT (constant)
```

The `lsqlite3.CONSTRAINT` error code means that an SQL constraint
violation occurred while trying to process an SQL statement. Additional
information about the failed constraint can be found by consulting the
accompanying error message (returned via `errmsg()`) or by looking at
the extended error code.
The `lsqlite3.CONSTRAINT` code can also be used as the return value from
the `xBestIndex()` method of a virtual table implementation. When
`xBestIndex()` returns `lsqlite3.CONSTRAINT`, that indicates that the
particular combination of inputs submitted to `xBestIndex()` cannot
result in a usable query plan and should not be given further
consideration.

### CORRUPT

```lua
lsqlite3.CORRUPT (constant)
```

The `lsqlite3.CORRUPT` result code indicates that the database file has
been corrupted. See [How To Corrupt Your Database Files](https://www.sqlite.org/lockingv3.html#how_to_corrupt)
for further discussion on how corruption can occur.

### DONE

```lua
lsqlite3.DONE (constant)
```

The `lsqlite3.DONE` result code indicates that an operation has
completed. The `lsqlite3.DONE` result code is most commonly seen as a
return value from `step()` indicating that the SQL statement has run to
completion, but `lsqlite3.DONE` can also be returned by other multi-step
interfaces.

### EMPTY

```lua
lsqlite3.EMPTY (constant)
```

The `lsqlite3.EMPTY` result code is not currently used.

### ERROR

```lua
lsqlite3.ERROR (constant)
```

The `lsqlite3.ERROR` result code is a generic error code that is used
when no other more specific error code is available.

### FORMAT

```lua
lsqlite3.FORMAT (constant)
```

The `lsqlite3.FORMAT` error code is not currently used by SQLite.

### FULL

```lua
lsqlite3.FULL (constant)
```

The `lsqlite3.FULL` result code indicates that a write could not
complete because the disk is full. Note that this error can occur when
trying to write information into the main database file, or it can also
occur when writing into temporary disk files.
Sometimes applications encounter this error even though there is an
abundance of primary disk space because the error occurs when writing
into temporary disk files on a system where temporary files are stored
on a separate partition with much less space that the primary disk.

### INTERNAL

```lua
lsqlite3.INTERNAL (constant)
```

The `lsqlite3.INTERNAL` result code indicates an internal malfunction.
In a working version of SQLite, an application should never see this
result code. If application does encounter this result code, it shows
that there is a bug in the database engine.
SQLite does not currently generate this result code. However,
application-defined SQL functions or virtual tables, or VFSes, or other
extensions might cause this result code to be returned.

### INTERRUPT

```lua
lsqlite3.INTERRUPT (constant)
```

The `lsqlite3.INTERRUPT` result code indicates that an operation was
interrupted by the `sqlite3_interrupt()` interface. See also:
`lsqlite3.ABORT`

### IOERR

```lua
lsqlite3.IOERR (constant)
```

The `lsqlite3.IOERR` result code says that the operation could not
finish because the operating system reported an I/O error.
A full disk drive will normally give an `lsqlite3.FULL` error rather
than an `lsqlite3.IOERR` error.
There are many different extended result codes for I/O errors that
identify the specific I/O operation that failed.

### LOCKED

```lua
lsqlite3.LOCKED (constant)
```

The `lsqlite3.LOCKED` result code indicates that a write operation could
not continue because of a conflict within the same database connection
or a conflict with a different database connection that uses a shared
cache.
For example, a DROP TABLE statement cannot be run while another thread
is reading from that table on the same database connection because
dropping the table would delete the table out from under the concurrent
reader.
The `lsqlite3.LOCKED` result code differs from `lsqlite3.BUSY` in that
`lsqlite3.LOCKED` indicates a conflict on the same database connection
(or on a connection with a shared cache) whereas `lsqlite3.BUSY`
indicates a conflict with a different database connection, probably in
a different process.

### MISMATCH

```lua
lsqlite3.MISMATCH (constant)
```

SQLite is normally very forgiving about mismatches between the type of a
value and the declared type of the container in which that value is to
be stored. For example, SQLite allows the application to store a large
BLOB in a column with a declared type of BOOLEAN. But in a few cases,
SQLite is strict about types. The `lsqlite3.MISMATCH` error is returned
in those few cases when the types do not match.
The rowid of a table must be an integer. Attempt to set the rowid to
anything other than an integer (or a NULL which will be automatically
converted into the next available integer rowid) results in an
`lsqlite3.MISMATCH` error.

### MISUSE

```lua
lsqlite3.MISUSE (constant)
```

The `lsqlite3.MISUSE` return code might be returned if the application
uses any SQLite interface in a way that is undefined or unsupported. For
example, using a prepared statement after that prepared statement has
been finalized might result in an `lsqlite3.MISUSE` error.
SQLite tries to detect misuse and report the misuse using this result
code. However, there is no guarantee that the detection of misuse will
be successful. Misuse detection is probabilistic. Applications should
never depend on an `lsqlite3.MISUSE` return value.
If SQLite ever returns `lsqlite3.MISUSE` from any interface, that means
that the application is incorrectly coded and needs to be fixed. Do not
ship an application that sometimes returns `lsqlite3.MISUSE` from a
standard SQLite interface because that application contains potentially
serious bugs.

### NOLFS

```lua
lsqlite3.NOLFS (constant)
```

The `lsqlite3.NOLFS` error can be returned on systems that do not
support large files when the database grows to be larger than what the
filesystem can handle. "NOLFS" stands for "NO Large File Support".

### NOMEM

```lua
lsqlite3.NOMEM (constant)
```

The `lsqlite3.NOMEM` result code indicates that SQLite was unable to
allocate all the memory it needed to complete the operation. In other
words, an internal call to `sqlite3_malloc()` or `sqlite3_realloc()` has
failed in a case where the memory being allocated was required in order
to continue the operation.

### NOTADB

```lua
lsqlite3.NOTADB (constant)
```

When attempting to open a file, the `lsqlite3.NOTADB` error indicates
that the file being opened does not appear to be an SQLite database
file.

### NOTFOUND

```lua
lsqlite3.NOTFOUND (constant)
```

The `lsqlite3.NOTFOUND` result code is exposed in three ways:
`lsqlite3.NOTFOUND` can be returned by the `sqlite3_file_control()`
interface to indicate that the file control opcode passed as the third
argument was not recognized by the underlying VFS.
`lsqlite3.NOTFOUND` can also be returned by the xSetSystemCall() method
of an sqlite3_vfs object.
`lsqlite3.NOTFOUND` an be returned by sqlite3_vtab_rhs_value() to
indicate that the right-hand operand of a constraint is not available
to the xBestIndex method that made the call.
The `lsqlite3.NOTFOUND` result code is also used internally by the
SQLite implementation, but those internal uses are not exposed to the
application.

### OK

```lua
lsqlite3.OK (constant)
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
The `lsqlite3.OK` result code means that the operation was successful
and that there were no errors. Most other result codes indicate an
error.

### OPEN_CREATE

```lua
lsqlite3.OPEN_CREATE (constant)
```

The database is created if it does not already exist.

### OPEN_FULLMUTEX

```lua
lsqlite3.OPEN_FULLMUTEX (constant)
```

The new database connection will use the "serialized" threading mode.
This means the multiple threads can safely attempt to use the same
database connection at the same time. (Mutexes will block any actual
concurrency, but in this mode there is no harm in trying.)

### OPEN_MEMORY

```lua
lsqlite3.OPEN_MEMORY (constant)
```

The database will be opened as an in-memory database. The database is
named by the "filename" argument for the purposes of cache-sharing, if
shared cache mode is enabled, but the "filename" is otherwise ignored.

### OPEN_NOMUTEX

```lua
lsqlite3.OPEN_NOMUTEX (constant)
```

The new database connection will use the "multi-thread" threading mode.
This means that separate threads are allowed to use SQLite at the same
time, as long as each thread is using a different database connection.

### OPEN_PRIVATECACHE

```lua
lsqlite3.OPEN_PRIVATECACHE (constant)
```

The database is opened with shared cache disabled, overriding the
default shared cache setting provided by sqlite3_enable_shared_cache().

### OPEN_READONLY

```lua
lsqlite3.OPEN_READONLY (constant)
```

The database is opened in read-only mode. If the database does not
already exist, an error is returned.

### OPEN_READWRITE

```lua
lsqlite3.OPEN_READWRITE (constant)
```

The database is opened for reading and writing if possible, or reading
only if the file is write protected by the operating system. In either
case the database must already exist, otherwise an error is returned.

### OPEN_SHAREDCACHE

```lua
lsqlite3.OPEN_SHAREDCACHE (constant)
```

The database is opened shared cache enabled, overriding the default
shared cache setting provided by sqlite3_enable_shared_cache(). The use
of shared cache mode is discouraged and hence shared cache capabilities
may be omitted from many builds of SQLite. In such cases, this option is
a no-op.

### OPEN_URI

```lua
lsqlite3.OPEN_URI (constant)
```

The filename can be interpreted as a URI if this flag is set. See
https://www.sqlite.org/c3ref/open.html

### PERM

```lua
lsqlite3.PERM (constant)
```

The `lsqlite3.PERM` result code indicates that the requested access mode
for a newly created database could not be provided.

### PROTOCOL

```lua
lsqlite3.PROTOCOL (constant)
```

The `lsqlite3.PROTOCOL` result code indicates a problem with the file
locking protocol used by SQLite. The `lsqlite3.PROTOCOL` error is
currently only returned when using WAL mode and attempting to start a
new transaction. There is a race condition that can occur when two
separate database connections both try to start a transaction at the
same time in WAL mode. The loser of the race backs off and tries again,
after a brief delay. If the same connection loses the locking race
dozens of times over a span of multiple seconds, it will eventually give
up and return `lsqlite3.PROTOCOL`. The `lsqlite3.PROTOCOL` error should
appear in practice very, very rarely, and only when there are many
separate processes all competing intensely to write to the same
database.

### RANGE

```lua
lsqlite3.RANGE (constant)
```

The `lsqlite3.RANGE` error indices that the parameter number argument to
one of the `bind` routines or the column number in one of the `column`
routines is out of range.

### READONLY

```lua
lsqlite3.READONLY (constant)
```

The `lsqlite3.READONLY` result code is returned when an attempt is made
to alter some data for which the current database connection does not
have write permission.

### ROW

```lua
lsqlite3.ROW (constant)
```

The `lsqlite3.ROW` result code returned by sqlite3_step() indicates that
another row of output is available.

### SCHEMA

```lua
lsqlite3.SCHEMA (constant)
```

The `lsqlite3.SCHEMA` result code indicates that the database schema has
changed. This result code can be returned from `Statement:step()`. If
the database schema was changed by some other process in between the
time that the statement was prepared and the time the statement was run,
this error can result.
The statement is automatically re-prepared if the schema changes, up to
`SQLITE_MAX_SCHEMA_RETRY` times (default: 50). The `step()` interface
will only return `lsqlite3.SCHEMA` back to the application if the
failure persists after these many retries.

### TOOBIG

```lua
lsqlite3.TOOBIG (constant)
```

The `lsqlite3.TOOBIG` error code indicates that a string or BLOB was too
large. The default maximum length of a string or BLOB in SQLite is
1,000,000,000 bytes. This maximum length can be changed at compile-time
using the `SQLITE_MAX_LENGTH` compile-time option. The `lsqlite3.TOOBIG`
error results when SQLite encounters a string or BLOB that exceeds the
compile-time limit.
The `lsqlite3.TOOBIG` error code can also result when an oversized SQL
statement is passed into one of the `db:prepare()` interface. The
maximum length of an SQL statement defaults to a much smaller value of
1,000,000,000 bytes.

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
