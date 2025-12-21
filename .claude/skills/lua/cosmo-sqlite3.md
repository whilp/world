# cosmo.sqlite3

SQLite3 database interface for Lua, provided by the `cosmo` module.

SQLite is a self-contained, serverless SQL database engine. This module provides complete Lua bindings for SQLite3.

## Usage

```lua
local cosmo = require("cosmo")
local sqlite3 = cosmo.sqlite3
```

## Module functions

### open

```lua
db, err = sqlite3.open(filename [, flags])
```

Open SQLite database file. Returns database object on success, or `nil, err` on failure.

```lua
local db = sqlite3.open('data.db')
if not db then
  print("Failed to open database")
end
```

Flags (optional):
- `sqlite3.OPEN_READONLY` - read-only access
- `sqlite3.OPEN_READWRITE` - read-write access
- `sqlite3.OPEN_CREATE` - create if doesn't exist
- `sqlite3.OPEN_URI` - interpret filename as URI

### open_memory

```lua
db = sqlite3.open_memory()
```

Open in-memory database. Useful for temporary data or testing.

```lua
local db = sqlite3.open_memory()
db:exec('CREATE TABLE temp (x INTEGER)')
```

### version

```lua
version_string = sqlite3.version()
```

Get SQLite library version.

```lua
print(sqlite3.version())  -- prints "3.40.0"
```

### lversion

```lua
binding_version = sqlite3.lversion()
```

Get Lua binding version.

## Database methods

### exec

```lua
ok, err = db:exec(sql)
```

Execute SQL statement(s). Returns `true` on success, or `nil, err` on failure.

```lua
db:exec('CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)')
db:exec([[
  INSERT INTO users VALUES (1, 'Alice');
  INSERT INTO users VALUES (2, 'Bob');
]])
```

### prepare

```lua
stmt, err = db:prepare(sql)
```

Prepare SQL statement for execution. Returns statement object on success, or `nil, err` on failure.

```lua
local stmt = db:prepare('SELECT * FROM users WHERE age > ?')
if stmt then
  stmt:bind(1, 25)
  -- use statement
  stmt:finalize()
end
```

### execute

```lua
ok, err = db:execute(sql)
```

Alias for `exec`.

### nrows

```lua
iterator = db:nrows(sql)
```

Execute query and iterate rows as named fields (table with column names as keys).

```lua
for row in db:nrows('SELECT id, name FROM users') do
  print(row.id, row.name)
end
```

### rows

```lua
iterator = db:rows(sql)
```

Execute query and iterate rows as arrays (numeric indices).

```lua
for row in db:rows('SELECT id, name FROM users') do
  print(row[1], row[2])  -- id, name
end
```

### urows

```lua
iterator = db:urows(sql)
```

Execute query and iterate rows, unpacking values.

```lua
for id, name in db:urows('SELECT id, name FROM users') do
  print(id, name)
end
```

### close

```lua
ok = db:close()
```

Close database connection. Returns `true` on success.

```lua
db:close()
```

### isopen

```lua
bool = db:isopen()
```

Test if database connection is open.

### last_insert_rowid

```lua
rowid = db:last_insert_rowid()
```

Get ROWID of last inserted row.

```lua
db:exec([[INSERT INTO users (name) VALUES ('Charlie')]])
local id = db:last_insert_rowid()
print("Inserted with ID: " .. id)
```

### changes

```lua
count = db:changes()
```

Get number of rows changed by last statement.

```lua
db:exec([[UPDATE users SET name = 'Updated' WHERE id = 1]])
print("Changed " .. db:changes() .. " rows")
```

### total_changes

```lua
count = db:total_changes()
```

Get total number of rows changed since database opened.

### errcode

```lua
code = db:errcode()
```

Get error code from last operation.

### errmsg

```lua
message = db:errmsg()
```

Get error message from last operation.

### error_code

```lua
code = db:error_code()
```

Alias for `errcode`.

### error_message

```lua
message = db:error_message()
```

Alias for `errmsg`.

### interrupt

```lua
db:interrupt()
```

Interrupt long-running query.

### busy_timeout

```lua
db:busy_timeout(milliseconds)
```

Set timeout for busy handler.

```lua
db:busy_timeout(5000)  -- wait up to 5 seconds
```

### busy_handler

```lua
db:busy_handler(callback)
```

Set custom busy handler function.

### create_function

```lua
db:create_function(name, nargs, callback)
```

Create custom SQL function.

```lua
db:create_function('double', 1, function(x)
  return x * 2
end)

for row in db:urows('SELECT double(5)') do
  print(row)  -- prints 10
end
```

### create_aggregate

```lua
db:create_aggregate(name, nargs, step_func, final_func)
```

Create custom aggregate function.

### create_collation

```lua
db:create_collation(name, compare_func)
```

Create custom collation sequence.

### commit_hook

```lua
db:commit_hook(callback)
```

Set commit hook callback.

### rollback_hook

```lua
db:rollback_hook(callback)
```

Set rollback hook callback.

### update_hook

```lua
db:update_hook(callback)
```

Set update hook callback.

### wal_hook

```lua
db:wal_hook(callback)
```

Set write-ahead log hook.

### wal_checkpoint

```lua
ok = db:wal_checkpoint(mode)
```

Checkpoint write-ahead log.

### db_filename

```lua
path = db:db_filename(name)
```

Get database file path.

### readonly

```lua
bool = db:readonly(name)
```

Test if database is read-only.

### close_vm

```lua
db:close_vm()
```

Close all prepared statements.

## Statement methods

### bind

```lua
stmt:bind(index, value)
```

Bind value to parameter by index (1-based).

```lua
local stmt = db:prepare('INSERT INTO users VALUES (?, ?)')
stmt:bind(1, 100)
stmt:bind(2, 'Name')
stmt:step()
stmt:finalize()
```

### bind_values

```lua
stmt:bind_values(value1, value2, ...)
```

Bind multiple values in order.

```lua
local stmt = db:prepare('INSERT INTO users VALUES (?, ?)')
stmt:bind_values(100, 'Name')
stmt:step()
stmt:finalize()
```

### bind_names

```lua
stmt:bind_names(table)
```

Bind values by parameter names.

```lua
local stmt = db:prepare('INSERT INTO users VALUES (:id, :name)')
stmt:bind_names({id = 100, name = 'Name'})
stmt:step()
stmt:finalize()
```

### bind_blob

```lua
stmt:bind_blob(index, blob)
```

Bind blob (binary data) to parameter.

### step

```lua
result = stmt:step()
```

Execute statement step. Returns:
- `sqlite3.ROW` if row available
- `sqlite3.DONE` if complete
- Error code on failure

```lua
local stmt = db:prepare('SELECT * FROM users')
while stmt:step() == sqlite3.ROW do
  local id = stmt:get_value(0)
  local name = stmt:get_value(1)
  print(id, name)
end
stmt:finalize()
```

### reset

```lua
stmt:reset()
```

Reset statement to initial state (can re-bind and re-execute).

```lua
local stmt = db:prepare('INSERT INTO users VALUES (?, ?)')

stmt:bind_values(1, 'Alice')
stmt:step()
stmt:reset()

stmt:bind_values(2, 'Bob')
stmt:step()
stmt:finalize()
```

### finalize

```lua
stmt:finalize()
```

Finalize and free statement.

### get_value

```lua
value = stmt:get_value(index)
```

Get column value by index (0-based).

### get_values

```lua
values = stmt:get_values()
```

Get all column values as array.

### get_named_values

```lua
values = stmt:get_named_values()
```

Get all column values as table (column names as keys).

### get_uvalues

```lua
val1, val2, ... = stmt:get_uvalues()
```

Get all column values unpacked.

### columns

```lua
count = stmt:columns()
```

Get number of columns in result.

### get_name

```lua
name = stmt:get_name(index)
```

Get column name by index (0-based).

### get_names

```lua
names = stmt:get_names()
```

Get all column names as array.

### get_unames

```lua
name1, name2, ... = stmt:get_unames()
```

Get all column names unpacked.

### get_type

```lua
type = stmt:get_type(index)
```

Get column type by index. Returns one of:
- `sqlite3.INTEGER`
- `sqlite3.FLOAT`
- `sqlite3.TEXT`
- `sqlite3.BLOB`
- `sqlite3.NULL`

### get_types

```lua
types = stmt:get_types()
```

Get all column types as array.

### get_named_types

```lua
types = stmt:get_named_types()
```

Get all column types as table (column names as keys).

### nrows

```lua
iterator = stmt:nrows()
```

Iterate statement rows as named tables.

```lua
local stmt = db:prepare('SELECT * FROM users')
for row in stmt:nrows() do
  print(row.id, row.name)
end
stmt:finalize()
```

### rows

```lua
iterator = stmt:rows()
```

Iterate statement rows as arrays.

### urows

```lua
iterator = stmt:urows()
```

Iterate statement rows, unpacking values.

### bind_parameter_count

```lua
count = stmt:bind_parameter_count()
```

Get number of bound parameters.

### bind_parameter_name

```lua
name = stmt:bind_parameter_name(index)
```

Get parameter name by index (1-based).

### last_insert_rowid

```lua
rowid = stmt:last_insert_rowid()
```

Get ROWID of last insert.

### readonly

```lua
bool = stmt:readonly()
```

Test if statement is read-only.

### isopen

```lua
bool = stmt:isopen()
```

Test if statement is active.

## Constants

### Result codes

- `DONE` - statement completed
- `ROW` - row available
- `OK` - operation successful
- `ERROR` - generic error
- `BUSY` - database locked
- `CONSTRAINT` - constraint violation
- `ABORT` - operation aborted
- `INTERRUPT` - operation interrupted

### Data types

- `INTEGER` - integer value
- `FLOAT` - floating point value
- `TEXT` - text string
- `BLOB` - binary data
- `NULL` - null value

### Open flags

- `OPEN_READONLY` - read-only access
- `OPEN_READWRITE` - read-write access
- `OPEN_CREATE` - create if doesn't exist

## Examples

### Basic usage

```lua
local sqlite3 = require('cosmo').sqlite3

-- Open database
local db = sqlite3.open('mydata.db')

-- Create table
db:exec([[
  CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    email TEXT UNIQUE,
    age INTEGER
  )
]])

-- Insert data
db:exec([[INSERT INTO users (name, email, age) VALUES ('Alice', 'alice@example.com', 30)]])
db:exec([[INSERT INTO users (name, email, age) VALUES ('Bob', 'bob@example.com', 25)]])

-- Query data
print("All users:")
for row in db:nrows('SELECT * FROM users') do
  print(string.format("  %d: %s (%s), age %d", row.id, row.name, row.email, row.age))
end

-- Close database
db:close()
```

### Prepared statements

```lua
local db = sqlite3.open('mydata.db')

-- Prepare insert statement
local insert = db:prepare('INSERT INTO users (name, email, age) VALUES (?, ?, ?)')

-- Insert multiple rows
local users = {
  {'Charlie', 'charlie@example.com', 35},
  {'Diana', 'diana@example.com', 28},
  {'Eve', 'eve@example.com', 32}
}

for _, user in ipairs(users) do
  insert:bind_values(user[1], user[2], user[3])
  insert:step()
  insert:reset()
end

insert:finalize()

print("Inserted " .. db:total_changes() .. " users")
db:close()
```

### Transactions

```lua
local db = sqlite3.open('mydata.db')

-- Begin transaction
db:exec('BEGIN TRANSACTION')

-- Multiple operations
local ok, err = pcall(function()
  db:exec([[INSERT INTO users (name, email, age) VALUES ('Frank', 'frank@example.com', 40)]])
  db:exec([[INSERT INTO users (name, email, age) VALUES ('Grace', 'grace@example.com', 29)]])
  db:exec([[UPDATE users SET age = age + 1 WHERE name = 'Alice']])
end)

-- Commit or rollback
if ok then
  db:exec('COMMIT')
  print("Transaction committed")
else
  db:exec('ROLLBACK')
  print("Transaction rolled back: " .. tostring(err))
end

db:close()
```

### Custom functions

```lua
local db = sqlite3.open_memory()

-- Create custom function
db:create_function('square', 1, function(x)
  return x * x
end)

db:exec('CREATE TABLE numbers (n INTEGER)')
db:exec('INSERT INTO numbers VALUES (1), (2), (3), (4), (5)')

-- Use custom function
print("Squares:")
for n, sq in db:urows('SELECT n, square(n) FROM numbers') do
  print(string.format("  %d^2 = %d", n, sq))
end

db:close()
```

### Error handling

```lua
local db = sqlite3.open('mydata.db')

-- Handle constraint violation
local ok, err = pcall(function()
  db:exec([[INSERT INTO users (name, email, age) VALUES ('Alice', 'alice@example.com', 30)]])
end)

if not ok then
  print("Error: " .. db:errmsg())
  print("Error code: " .. db:errcode())
end

-- Handle with prepared statement
local stmt = db:prepare('INSERT INTO users (name, email, age) VALUES (?, ?, ?)')
stmt:bind_values('Alice', 'alice@example.com', 30)

local result = stmt:step()
if result == sqlite3.CONSTRAINT then
  print("Constraint violation: duplicate email")
elseif result == sqlite3.DONE then
  print("Insert successful")
else
  print("Error: " .. db:errmsg())
end

stmt:finalize()
db:close()
```

### In-memory database

```lua
local db = sqlite3.open_memory()

-- Perfect for temporary data
db:exec([[
  CREATE TABLE temp (
    key TEXT PRIMARY KEY,
    value TEXT
  )
]])

db:exec([[INSERT INTO temp VALUES ('foo', 'bar')]])
db:exec([[INSERT INTO temp VALUES ('hello', 'world')]])

for key, value in db:urows('SELECT * FROM temp') do
  print(key .. " = " .. value)
end

db:close()
-- Database is destroyed when closed
```

### Named parameters

```lua
local db = sqlite3.open('mydata.db')

local stmt = db:prepare([[
  INSERT INTO users (name, email, age)
  VALUES (:name, :email, :age)
]])

stmt:bind_names({
  name = 'Henry',
  email = 'henry@example.com',
  age = 45
})

stmt:step()
stmt:finalize()
db:close()
```

### Iterating large results

```lua
local db = sqlite3.open('large_data.db')

-- Memory-efficient iteration
local stmt = db:prepare('SELECT * FROM large_table WHERE category = ?')
stmt:bind(1, 'important')

while stmt:step() == sqlite3.ROW do
  local id = stmt:get_value(0)
  local data = stmt:get_value(1)

  -- Process one row at a time
  process_row(id, data)
end

stmt:finalize()
db:close()
```

## Performance tips

- Use transactions for multiple inserts (much faster)
- Use prepared statements for repeated queries
- Create indexes on frequently queried columns
- Use `VACUUM` periodically to reclaim space
- Consider WAL mode for concurrent access: `PRAGMA journal_mode=WAL`
- Use `ANALYZE` to update query optimizer statistics

## Notes

- SQLite is single-file, serverless, zero-configuration
- Supports full SQL92
- ACID compliant with transactions
- Cross-platform and cross-architecture
- Database files are portable between platforms
- Perfect for embedded applications and local storage

## References

- [SQLite Documentation](https://www.sqlite.org/docs.html)
- [SQLite SQL Syntax](https://www.sqlite.org/lang.html)
- [SQLite Performance](https://www.sqlite.org/speed.html)
