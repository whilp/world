-- Query every second
if not date then
    statusd_date={ interval=1*1000 }
end

local function updateDate()
    local date=os.date('%a %d %b %H:%M:%S UTC%z %Y')

    if date ~= nil then 
        return string.format("[%s]", date)
    else
        return ""
    end
end

local date_timer

local function send_update()
    statusd.inform("date", updateDate())
    date_timer:set(statusd_date.interval, send_update)
end

date_timer=statusd.create_timer()
send_update()
