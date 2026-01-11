-- type declarations for argparse

local record Argument
  args: function(self: Argument, n: string | number): Argument
  count: function(self: Argument, n: string | number): Argument
  target: function(self: Argument, name: string): Argument
  default: function(self: Argument, value: any): Argument
  convert: function(self: Argument, fn: function(string): any): Argument
  choices: function(self: Argument, list: {string}): Argument
  hidden: function(self: Argument, hidden?: boolean): Argument
end

local record Option
  args: function(self: Option, n: string | number): Option
  count: function(self: Option, n: string | number): Option
  target: function(self: Option, name: string): Option
  default: function(self: Option, value: any): Option
  convert: function(self: Option, fn: function(string): any): Option
  choices: function(self: Option, list: {string}): Option
  hidden: function(self: Option, hidden?: boolean): Option
  argname: function(self: Option, name: string): Option
end

local record Flag
  target: function(self: Flag, name: string): Flag
  count: function(self: Flag, n: string | number): Flag
  hidden: function(self: Flag, hidden?: boolean): Flag
  action: function(self: Flag, fn: function()): Flag
end

local record Command
  argument: function(self: Command, name: string, description?: string): Argument
  option: function(self: Command, name: string, description?: string): Option
  flag: function(self: Command, name: string, description?: string): Flag
  command: function(self: Command, name: string, description?: string): Command
  require_command: function(self: Command, require?: boolean): Command
  parse: function(self: Command, args?: {string}): {string:any}
  pparse: function(self: Command, args?: {string}): boolean, {string:any} | string
  get_help: function(self: Command): string
  add_help: function(self: Command, help?: boolean | string): Command
  epilog: function(self: Command, text: string): Command
  group: function(self: Command, name: string): Command
end

local record Parser
  argument: function(self: Parser, name: string, description?: string): Argument
  option: function(self: Parser, name: string, description?: string): Option
  flag: function(self: Parser, name: string, description?: string): Flag
  command: function(self: Parser, name: string, description?: string): Command
  require_command: function(self: Parser, require?: boolean): Parser
  parse: function(self: Parser, args?: {string}): {string:any}
  pparse: function(self: Parser, args?: {string}): boolean, {string:any} | string
  get_help: function(self: Parser): string
  add_help: function(self: Parser, help?: boolean | string): Parser
  epilog: function(self: Parser, text: string): Parser
  group: function(self: Parser, name: string): Parser
end

local record argparse
  metamethod __call: function(self: argparse, name?: string, description?: string): Parser
end

return argparse
