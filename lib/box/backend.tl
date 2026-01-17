-- box/backend.d.tl: type definitions for box backends

local record Result
  ok: boolean
  err: string
end

local record Backend
  name: string
  new: function(name: string, ...: string): Result
  ssh: function(name: string, ...: string): Result
  exec: function(name: string, cmd: string): Result
  upload: function(name: string, src: string, dst: string): Result
  destroy: function(name: string): Result
  list: function(): Result
end

local record Env
  github: {string: any}
  claude: {string: any}
end

return {
  Result = Result,
  Backend = Backend,
  Env = Env,
}
