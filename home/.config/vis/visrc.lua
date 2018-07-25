-- load standard vis module, providing parts of the Lua API
require('vis')
require('plugins/complete-filename')
require('plugins/complete-word')
require('plugins/filetype')
require('plugins/textobject-lexer')


vis.events.subscribe(vis.events.INIT, function()
    vis:command('set theme solarized')
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
    vis:command('set number')
    vis:command('set autoindent')
    vis:command('set colorcolumn 80')
    vis:command('set show-tabs')
    vis:command('set horizon 128')
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
    python = function()
        vis:command("set ai")
        vis:command("set et")
        vis:command("set tw 4")
    end
    bazel = function()
        vis:command("set syntax python")
        python()
    end
    local exts = {
        py = python,
        lua = function()
            vis:command("set tw 4")
            vis:command("set et")
            vis:command("set ai")
        end,
        bazel = bazel,
        bzl = bazel,
    }
    fn = exts[ext(win.file.name)]
    if fn ~= nil then fn() end
end)

ext = function(filename)
    if filename == nil then return nil end
    got = filename:find("%.%S+$")
    if got == nil then return nil end
    i, j = got
    return filename:sub(i+1, j)
end
