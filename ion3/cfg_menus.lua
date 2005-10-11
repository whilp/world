--
-- Ion menu definitions
--


-- Main menu
defmenu("mainmenu", {
    submenu("Programs",         "appmenu"),
    menuentry("Lock screen",
              "ioncore.exec_on(_, ioncore.lookup_script('ion-lock'))"),
    menuentry("Help",           "mod_query.query_man(_)"),
    menuentry("About Ion",      "mod_query.show_about_ion(_)"),
    submenu("Styles",           "stylemenu"),
    submenu("Debian",           "Debian"),
    submenu("Session",          "sessionmenu"),
})


-- Application menu
defmenu("appmenu", {
    menuentry("Terminal",       "ioncore.exec_on(_, 'x-terminal-emulator')"),
    menuentry("Browser",        "ioncore.exec_on(_, 'sensible-browser')"),

    menuentry("Run...",         "mod_query.query_exec(_)"),
})


-- Session control menu
defmenu("sessionmenu", {
    menuentry("Save",           "ioncore.snapshot()"),
    menuentry("Restart",        "ioncore.restart()"),
    menuentry("Restart PWM3",    "ioncore.restart_other('pwm3')"),
    menuentry("Restart TWM",    "ioncore.restart_other('twm')"),
    menuentry("Exit",           "ioncore.shutdown()"),
})


-- Context menu (frame/client window actions)
defctxmenu("WFrame", {
    menuentry("Close",          "WRegion.rqclose_propagate(_, _sub)"),
    menuentry("Kill",           "WClientWin.kill(_sub)",
                                "_sub:WClientWin"),
    menuentry("(Un)tag",        "WRegion.toggle_tag(_sub)",
                                "_sub:non-nil"),
    menuentry("Attach tagged",  "WFrame.attach_tagged(_)"),
    menuentry("Clear tags",     "ioncore.clear_tags()"),
    menuentry("Window info",    "mod_query.show_clientwin(_, _sub)",
                                "_sub:WClientWin"),
})

-- Auto-generated Debian menu definitions
if os.execute("test -x /usr/bin/update-menus") == 0 then
    if ioncore.is_i18n() then
        dopath("debian-menu-i18n")
    else
        dopath("debian-menu")
    end
end
