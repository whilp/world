xsetroot -solid white

# switch caps and control
cat <<EOF | xmodmap -
    remove Lock = Caps_Lock
    remove Control = Control_L
    keysym Control_L = Caps_Lock
    keysym Caps_Lock = Control_L
    add Lock = Caps_Lock
    add Control = Control_L
    keycode 234 = Page_Down
    keycode 233 = Page_Up
EOF

# Terminals
[ -r "${HOME}/.xinitrc.local" ] && . ${HOME}/.xinitrc.local

xset b off
[ -x "$(which unclutter 2>/dev/null)" ] && unclutter -idle 5 -jitter 40&

#exec ion3
#exec matchbox-session
#exec wmii
#exec dwm
#exec startkde

[ -p ~/.dwm/fifo ] || mkfifo ~/.dwm/fifo 2>/dev/null
[ -x ~/.dwm/status ] && ~/.dwm/status -f &
exec awesome <> ~/.dwm/fifo
