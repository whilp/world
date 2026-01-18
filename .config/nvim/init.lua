-- nvim bootstrap: load teal, then require teal config
local config_dir = vim.fn.stdpath("config")
local home = vim.fn.fnamemodify(config_dir, ":h:h")
package.path = config_dir .. "/?.lua;" .. config_dir .. "/?/init.lua;" .. home .. "/lib/?.lua;" .. home .. "/lib/3p/?.lua;" .. package.path

require("tl").loader()
require("init")  -- loads .config/nvim/init.tl
