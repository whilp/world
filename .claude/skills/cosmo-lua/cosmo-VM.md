# cosmo.VM

### type

```lua
VM.type
```

This function causes any pending database operation to abort and return at
the next opportunity.
Each row in an SQLite table has a unique 64-bit signed integer key called
the rowid. This id is always available as an undeclared column named ROWID,
OID, or _ROWID_. If the table has a column of type INTEGER PRIMARY KEY then
that column is another alias for the rowid.
If an INSERT occurs within a trigger, then the rowid of the inserted row is
returned as long as the trigger is running. Once the trigger terminates, the
value returned reverts to the last value inserted before the trigger fired.
Creates an iterator that returns the successive rows selected by the
SQL statement given in string `sql`. Each call to the iterator
returns a table in which the named fields correspond to the columns
in the database. Here is an example:
db:exec[=[
CREATE TABLE numbers(num1,num2);
INSERT INTO numbers VALUES(1,11);
INSERT INTO numbers VALUES(2,22);
INSERT INTO numbers VALUES(3,33);
]=]
for a in db:nrows('SELECT * FROM numbers') do table.print(a) end
This script prints:
num2: 11
num1: 1
num2: 22
num1: 2
num2: 33
num1: 3
This function compiles the SQL statement in string sql into an internal
representation and returns this as userdata. The returned object should be
used for all further method calls in connection with this specific SQL
statement.
See http://lua.sqlite.org/index.cgi/doc/tip/doc/lsqlite3.wiki#methods_for_prepared_statements.
This function installs a rollback_hook callback handler.
See: `db:commit_hook` and `db:update_hook`
Creates an iterator that returns the successive rows selected by the SQL
statement given in string `sql`. Each call to the iterator returns a table in
which the numerical indices 1 to n correspond to the selected columns 1 to n in
the database. Here is an example:
db:exec[=[
CREATE TABLE numbers(num1,num2);
INSERT INTO numbers VALUES(1,11);
INSERT INTO numbers VALUES(2,22);
INSERT INTO numbers VALUES(3,33);
]=]
for a in db:rows('SELECT * FROM numbers') do table.print(a) end
This script prints:
1: 1
2: 11
1: 2
2: 22
1: 3
2: 33
Serialize a database to be restored later with `Database:deserialize`.
This includes UPDATE, INSERT and DELETE statements executed as part of trigger
programs. All changes are counted as soon as the statement that produces them
is completed by calling either `stmt:reset()` or `stmt:finalize()`.
This function installs an update_hook Data Change Notification
Callback handler. See: `db:commit_hook` and `db:rollback_hook`
whenever a row is updated, inserted or deleted. This callback
receives five arguments: the first is the `udata` argument used
when the callback was installed; the second is an integer
indicating the operation that caused the callback to be invoked
(one of `lsqlite3.UPDATE`, `lsqlite3.INSERT`, or
`lsqlite3.DELETE`). The third and fourth arguments are the
database and table name containing the affected row. The final
callback parameter is the rowid of the row. In the case of an
update, this is the rowid after the update takes place.
Creates an iterator that returns the successive rows selected by the SQL
statement given in string sql. Each call to the iterator returns the values
that correspond to the columns in the currently selected row.
Here is an example:
db:exec[=[
CREATE TABLE numbers(num1,num2);
INSERT INTO numbers VALUES(1,11);
INSERT INTO numbers VALUES(2,22);
INSERT INTO numbers VALUES(3,33);
]=]
for num1,num2 in db:urows('SELECT * FROM numbers') do print(num1,num2) end
This script prints:
1       11
2       22
3       33
Returned by `db:iterate_changeset`
Returned by `db:create_rebaser`.
Returned by `db:create_session`.
Closes the session. Further method calls on the session will throw errors.
After creating a prepared statement with `db:prepare()` the returned statement
object should be used for all further calls in connection with that statement.
Binds `value` to statement parameter `n`. If the type of `value` is
string it is bound as text. If the type of value is number, it is
bound as an integer or double depending on its subtype using
`lua_isinteger`. If `value` is a boolean then it is bound as `0` for
`false` or `1` for `true`. If `value` is `nil` or missing, any
previous binding is removed.
Binds string `blob` (which can be a binary string) as a blob to
statement parameter `n`.
Binds the values in `nametable` to statement parameters. If the
statement parameters are named (i.e., of the form `":AAA"` or
`"$AAA"`) then this function looks for appropriately named fields in
nametable; if the statement parameters are not named, it looks for
numerical fields 1 to the number of statement parameters.
When the statement parameters are of the forms `":AAA"` or `"?"`, then they are
assigned sequentially increasing numbers beginning with one, so the value
returned is the number of parameters. However if the same statement parameter
name is used multiple times, each occurrence is given the same number, so the
value returned is the number of unique statement parameter names.
If statement parameters of the form `"?NNN"` are used (where `NNN` is an
integer) then there might be gaps in the numbering and the value returned by
this interface is the index of the statement parameter with the largest index
value.
Statement parameters of the form `":AAA"` or `"@AAA"` or `"$VVV"` have a name
which is the string `":AAA"` or `"@AAA"` or `"$VVV"`. In other words, the
initial `":"` or `"$"` or `"@"` is included as part of the name. Parameters of
the form `"?"` or `"?NNN"` have no name. The first bound parameter has an index
of `1`. If the value `n` is out of range or if the `n`-th parameter is nameless,
then `nil` is returned.
Binds the given values to statement parameters.
This function frees the prepared statement.
This function resets the SQL statement, so that it is ready to be re-executed. Any statement variables that had values bound to them using the `stmt:bind*()` functions retain their values.
This function must be called to evaluate the (next iteration of the) prepared statement.
- `lsqlite3.BUSY`: the engine was unable to acquire the locks needed.
If the statement is a COMMIT or occurs outside of an explicit transaction,
then you can retry the statement. If the statement is not a COMMIT and occurs
within a explicit transaction then you should rollback the transaction before
continuing.
- `lsqlite3.DONE`: the statement has finished executing successfully.
`stmt:step()` should not be called again on this statement without first
calling `stmt:reset()` to reset the virtual machine back to the initial state.
- `lsqlite3.ROW`: this is returned each time a new row of data is ready for
processing by the caller. The values may be accessed using the column access
functions. `stmt:step()` can be called again to retrieve the next
row of data.
- `lsqlite3.ERROR`: a run-time error (such as a constraint violation) has
occurred. `stmt:step()` should not be called again. More
information may be found by calling `db:errmsg()`. A more specific error
code (can be obtained by calling `stmt:reset()`.
- `lsqlite3.MISUSE`: the function was called inappropriately, perhaps because
the statement has already been finalized or a previous call to `stmt:step()`
has returned `lsqlite3.ERROR` or `lsqlite3.DONE`.
Each iteration returns the values for the current row. This is the prepared
statement equivalent of `db:urows()`.
