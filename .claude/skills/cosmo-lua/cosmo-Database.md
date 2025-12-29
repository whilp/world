# cosmo.Database

### errcode

```lua
Database.errcode
```

Sets the result of a callback function to the error value in `err`.
Sets the result of a callback function to the integer `number`
Sets the result of a callback function to `nil`.
Sets the result of a callback function to the string in `str`.
Returns the userdata parameter given in the call to install the callback
function (see db:create_aggregate() and db:create_function() for details).
After opening a database with `lsqlite3.open()` or `lsqlite3.open_memory()`
the returned database object should be used for all further method calls in
connection with that database.
Sets or removes a busy handler for a database.
The handler function is called with two parameters: `udata` and the number
of (re-)tries for a pending transaction. It should return `nil`, `false` or
`0` if the transaction is to be aborted. All other values will result in
another attempt to perform the transaction. (See the SQLite documentation
for important hints about writing busy handlers.)
Sets a busy handler that waits for `milliseconds` if a transaction cannot proceed.
Calling this function will remove any busy handler set by `db:busy_handler()`;
calling it with an argument less than or equal to `0` will turn off all busy handlers.
Only changes that are directly specified by INSERT, UPDATE, or DELETE
statements are counted. Auxiliary changes caused by triggers are not
counted. Use `db:total_changes()` to find the total number of changes.
Closes a database. All SQL statements prepared using `db:prepare()` should
have been finalized before this function is called. The function returns
`lsqlite3.OK` on success or else a numerical error code.
Finalizes all statements that have not been explicitly finalized. If
`temponly` is `true`, only internal, temporary statements are finalized.
This function installs a `commit_hook` callback handler.
If `func` returns `false` or `nil` the COMMIT is allowed to proceed,
otherwise the COMMIT is converted to a ROLLBACK.
See: `db:rollback_hook` and `db:update_hook`
Concatenate a list of changesets.
This function creates an aggregate callback function. Aggregates perform an
operation over all rows in a query.
It should accept a function context (see Methods for callback contexts) plus
the same number of parameters as given in `nargs`.
It receives one argument, the function context.
The function context can be used inside the two callback functions to
communicate with SQLite3. Here is a simple example:
db:exec[=[
CREATE TABLE numbers(num1,num2);
INSERT INTO numbers VALUES(1,11);
INSERT INTO numbers VALUES(2,22);
INSERT INTO numbers VALUES(3,33);
]=]
local num_sum=0
local function oneRow(context, num)  -- add one column in all rows
num_sum = num_sum + num
end
local function afterLast(context)   -- return sum after last row has been processed
context:result_number(num_sum)
num_sum = 0
end
db:create_aggregate("do_the_sums", 1, oneRow, afterLast)
for sum in db:urows('SELECT do_the_sums(num1) FROM numbers') do print("Sum of col 1:",sum) end
for sum in db:urows('SELECT do_the_sums(num2) FROM numbers') do print("Sum of col 2:",sum) end
This prints:
Sum of col 1:   6
Sum of col 2:   66
This creates a collation callback. A collation callback is used to establish
a collation order, mostly for string comparisons and sorting purposes.
A simple example:
local function collate(s1,s2)
s1=s1:lower()
s2=s2:lower()
if s1==s2 then return 0
elseif s1<s2 then return -1
else return 1 end
end
db:exec[=[
CREATE TABLE test(id INTEGER PRIMARY KEY,content COLLATE CINSENS);
INSERT INTO test VALUES(NULL,'hello world');
INSERT INTO test VALUES(NULL,'Buenos dias');
INSERT INTO test VALUES(NULL,'HELLO WORLD');
]=]
db:create_collation('CINSENS',collate)
for row in db:nrows('SELECT * FROM test') do
print(row.id, row.content)
end
This function creates a callback function. Callback function are called by
SQLite3 once for every row in a query.
It should accept a function context (see Methods for callback contexts) plus
the same number of parameters as given in `nargs`.
Here is an example:
db:exec'CREATE TABLE test(col1,col2,col3)'
db:exec'INSERT INTO test VALUES(1,2,4)'
db:exec'INSERT INTO test VALUES(2,4,9)'
db:exec'INSERT INTO test VALUES(3,6,16)'
db:create_function('sum_cols',3,function(ctx,a,b,c)
ctx:result_number(a+b+c)
end))
for col1,col2,col3,sum in db:urows('SELECT *,sum_cols(col1,col2,col3) FROM test') do
util.printf('%2i+%2i+%2i=%2i\n',col1,col2,col3,sum)
end
If there is no attached database name on the database connection, then no value is
returned; if database name is a temporary or in-memory database, then an
empty string is returned.
Deserializes data from a string which was created by `db:serialize`.
See http://lua.sqlite.org/index.cgi/doc/tip/doc/lsqlite3.wiki#numerical_error_and_result_codes for details.

### exec

```lua
Database.exec
```

Compiles and executes the SQL statement(s) given in string sql. The statements
are simply executed one after the other and not stored. The function returns
`lsqlite3.OK` on success or else a numerical error code.
If one or more of the SQL statements are queries, then the callback function
specified in func is invoked once for each row of the query result (if func is
`nil`, no callback is invoked).
The callback receives four arguments:
-  `udata (the third parameter of the db:exec() call),
- the number of columns in the row
- a table with the column values
- another table with the column names.
The callback function should return `0`. If the callback returns a non-zero
value then the query is aborted, all subsequent SQL statements are skipped
and `db:exec()` returns `lsqlite3.ABORT`. Here is a simple example:
sql=[=[
CREATE TABLE numbers(num1,num2,str);
INSERT INTO numbers VALUES(1,11,"ABC");
INSERT INTO numbers VALUES(2,22,"DEF");
INSERT INTO numbers VALUES(3,33,"UVW");
INSERT INTO numbers VALUES(4,44,"XYZ");
SELECT * FROM numbers;
]=]
function showrow(udata,cols,values,names)
assert(udata=='test_udata')
print('exec:')
for i=1,cols do print('',names[i],values[i]) end
return 0
end
db:exec(sql,showrow,'test_udata')
