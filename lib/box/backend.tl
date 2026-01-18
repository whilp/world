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
  download: function(name: string, src: string, dst: string): Result
  destroy: function(name: string): Result
  list: function(): Result
end

local record Git
  name: string
  email: string
end

local record Bootstrap
  repo: string
  release: string
end

local record Env
  github: {string: any}
  git: Git
  claude: {string: any}
  bootstrap: Bootstrap
end

return {
  Result = Result,
  Backend = Backend,
  Git = Git,
  Bootstrap = Bootstrap,
  Env = Env,
}
