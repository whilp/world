-- Query every 10 seconds
if not mpd then
    statusd_mpd={ interval=10*1000 }
end

local function updateMpd()
    -- local f=io.popen('tunes query 2>/dev/null', 'r')
    local f=io.popen('mpc 2>/dev/null', 'r')
    local playing=f:read()

    if string.find(playing, '^volume:.*repeat:.*random') ~= 1 then 
        return string.format("[MPD: %s]", playing)
    else
        return "[MPD]"
    end
end

local mpd_timer

local function send_update()
    statusd.inform("mpd", updateMpd())
    mpd_timer:set(statusd_mpd.interval, send_update)
end

mpd_timer=statusd.create_timer()
send_update()
