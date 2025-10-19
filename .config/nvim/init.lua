-- nvim configuration entry point

-- Add lua paths
local home = vim.fn.expand("~")
package.path = home .. "/.local/lib/lua/?.lua;" .. home .. "/.local/lib/lua/3p/?.lua;" .. package.path