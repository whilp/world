if not bar then
    statusd_bar={ interval=1*1000 }
end

local function get_bar()
    local f=io.popen('bar','r')
    local bar_data=f:read()

    return bar_data
end

local bar_timer

local function update_bar()
    statusd.inform("bar", get_bar())
    bar_timer:set(statusd_bar.interval, update_bar)
end

bar_timer=statusd.create_timer()
update_bar()
