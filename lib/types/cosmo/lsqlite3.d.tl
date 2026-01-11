-- type declarations for cosmo.lsqlite3

local record Statement
  bind_values: function(self: Statement, ...: any)
  step: function(self: Statement): number
  nrows: function(self: Statement): function(): {string:any}
  finalize: function(self: Statement)
  reset: function(self: Statement)
end

local record Database
  exec: function(self: Database, sql: string): number
  prepare: function(self: Database, sql: string): Statement
  nrows: function(self: Database, sql: string): function(): {string:any}
  close: function(self: Database)
end

local record lsqlite3
  open: function(filename: string): Database
end

return lsqlite3
