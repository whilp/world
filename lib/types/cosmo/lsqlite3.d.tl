-- type declarations for cosmo.lsqlite3

local record lsqlite3
  record Statement
    bind_values: function(self: Statement, ...: any)
    step: function(self: Statement): number
    nrows: function(self: Statement): function(): {string:any}
    finalize: function(self: Statement)
    reset: function(self: Statement)
  end

  record Database
    exec: function(self: Database, sql: string): number
    prepare: function(self: Database, sql: string): Statement
    nrows: function(self: Database, sql: string): function(): {string:any}
    close: function(self: Database)
  end

  open: function(filename: string): Database
end

return lsqlite3
