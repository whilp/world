-- box/mac.tl: local mac backend (placeholder)
--
-- TODO: implement local mac backend for bootstrapping macs

local backend = require("box.backend")

local function not_implemented(): backend.Result
  return {ok = false, err = "mac backend not implemented"}
end

return {
  name = "mac",
  new = function(_: string, ...: string): backend.Result return not_implemented() end,
  ssh = function(_: string, ...: string): backend.Result return not_implemented() end,
  exec = function(_: string, _: string): backend.Result return not_implemented() end,
  upload = function(_: string, _: string, _: string): backend.Result return not_implemented() end,
  download = function(_: string, _: string, _: string): backend.Result return not_implemented() end,
  destroy = function(_: string): backend.Result return not_implemented() end,
  list = function(): backend.Result return not_implemented() end,
} as backend.Backend
