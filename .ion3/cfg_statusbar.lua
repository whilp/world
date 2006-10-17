--
-- Ion statusbar module configuration file
-- 


-- Create a statusbar
mod_statusbar.create{
    -- First screen, bottom left corner
    screen=0,
    pos='br',
    fullsize=false,
    systray=false,

    -- Template. Tokens %string are replaced with the value of the 
    -- corresponding meter. Currently supported meters are:
    --   date          date
    --   load          load average (1min, 5min, 15min)
    --   load_Nmin     N minute load average (N=1, 5, 15)
    --   mail_new      mail count (mbox format file $MAIL)
    --   mail_unread   mail count
    --   mail_total    mail count
    --   mail_*_new    mail count (from an alternate mail folder, see below)
    --   mail_*_unread mail count
    --   mail_*_total  mail count
    --
    -- Space preceded by % adds stretchable space for alignment of variable
    -- meter value widths. > before meter name aligns right using this 
    -- stretchable space , < left, and | centers.
    -- Meter values may be zero-padded to a width preceding the meter name.
    -- These alignment and padding specifiers and the meter name may be
    -- enclosed in braces {}.
    --
    -- %filler causes things on the marker's sides to be aligned left and
    -- right, respectively, and %systray is a placeholder for system tray
    -- windows and icons.
    --
    --template="[ %date || load:% %>load || mail:% %>mail_new/%>mail_total ] %filler%systray",
    --template="[ %date || load: %05load_1min || mail: %02mail_new/%02mail_total ] %filler%systray",
    -- template="[UP: %uptime][BATT: %laptopstatus_batterypercent][TEMP: %laptopstatus_temperature][NP: %mpd]%filler[vger][%date]"
    -- template="%bar"
    -- what's wrong with %mpd?
    --template="%mpd%uptime%hostname%date"
    template="%uptime%hostname%date"
}


-- Launch ion-statusd. This must be done after creating any statusbars
-- for necessary modules to be parsed from the templates.
mod_statusbar.launch_statusd{
}
