xsetroot -solid black

# switch caps and control
xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "remove Control = Control_L"
xmodmap -e "keysym Control_L = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Control_L"
xmodmap -e "add Lock = Caps_Lock"
xmodmap -e "add Control = Control_L"

# Terminals
[ -r "${HOME}/.xinitrc.local" ] && . ${HOME}/.xinitrc.local

xset b off
[ -x $(which unclutter 2>/dev/null) ] && unclutter -idle 5 -jitter 40&

#exec ion3
#exec matchbox-session
#exec wmii
#exec dwm
#exec startkde

[ -p ~/.dwm/fifo ] || mkfifo ~/.dwm/fifo 2>/dev/null
[ -x ${HOME}/.dwm/status ] && ${HOME}/.dwm/status &
exec dwm <> ~/.dwm/fifo
