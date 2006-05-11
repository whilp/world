-- Query every second
if not bar then
    statusd_bar={ interval=2*1000 }
end

local function addBar(oldbar, newthing)
    if oldbar == nil then
        return string.format("[%s]", newthing)
    else
        return string.format("%s[%s]", oldbar, newthing)
    end
end
    

local function updateBar()
    -- Bar format: [MPD: <song name>][UP: -- <uptime>][<hostname>][<date>]
    local bar_contents

    -- MPD check
    local f=io.popen('tunes query 2>/dev/null', 'r')
    local playing=f:read()

    if string.len(playing) > 0 then bar_contents = addBar(bar_contents, playing) end
    
    -- Uptime check
    local f=io.popen('uptime', 'r')
    local uptime=f:read()
    
    uptime = string.gsub(uptime, '^.+ up +', '')
    uptime = string.gsub(uptime, ', [ 0-9]+ users,.*', '')
    
    bar_contents = addBar(bar_contents, uptime)
    
    -- Hostname check
    local hostname=os.getenv("HOSTNAME")

    bar_contents = addBar(bar_contents, hostname)

    -- Date check
    local date=os.date('%a %d %b %H:%M:%S %z %Y')

    bar_contents = addBar(bar_contents, date)

    return bar_contents
end

local bar_timer

local function send_update()
    statusd.inform("bar", updateBar())
    bar_timer:set(statusd_bar.interval, send_update)
end

bar_timer=statusd.create_timer()
send_update()
