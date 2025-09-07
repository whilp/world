((string (string_start) @start
          (string_content) @sql
          (string_end) @end)
 (#match? @start "\"\"\"")
 (#match? @end "\"\"\"")
 (#match? @sql "^\\s*(with|select|insert|update|delete|create|drop|alter)"))

