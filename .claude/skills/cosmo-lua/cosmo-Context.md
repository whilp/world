# cosmo.Context

### result_number

```lua
Context.result_number
```

A callback context is available as a parameter inside the callback functions
`db:create_aggregate()` and `db:create_function()`. It can be used to get
further information about the state of a query.
Set the user-definable data field for callback funtions to `udata`.
Sets the result of a callback function to `res`. The type of the result
depends on the type of `res` and is either a number or a string or `nil`.
All other values will raise an error message.
Sets the result of a callback function to the binary string in blob.
Sets the result of a callback function to the value number.
