if not statusd_mpd then
  statusd_mpd={ interval=10*1000 }
end

local function get_mpd_status()
  local f=io.popen('MPD_HOST=localhost mpc --format "[[%artist - ]%title%|%name%|%file%]"','r')
  local playing=f:read()

  if playing == nil then
    return "mpd not running"
  elseif string.sub(playing, 1,7) == 'volume:' then
    return "No song playing"
  end
        
  return playing
end

local mpd_timer

local function update_mpd()
    statusd.inform("mpd", get_mpd_status())
    mpd_timer:set(statusd_mpd.interval, update_mpd)
end

-- Init
--get_inet_addr=get_inet_addr_fn()
mpd_timer=statusd.create_timer()
update_mpd()

