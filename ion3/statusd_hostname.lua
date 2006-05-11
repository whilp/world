-- Query every 10 minutes
if not hostname then
    statusd_hostname={ interval=600*1000 }
end

local function updateHostname()
    local hostname=os.getenv("HOSTNAME")

    if hostname ~= nil then 
        return string.format("[%s]", hostname)
    else
        return ""
    end
end

local hostname_timer

local function send_update()
    statusd.inform("hostname", updateHostname())
    hostname_timer:set(statusd_hostname.interval, send_update)
end

hostname_timer=statusd.create_timer()
send_update()
