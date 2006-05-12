xsetroot -solid black

[ -x $(which konqueror) -a -x $(which kdeinit) ] && kdeinit&

# switch caps and control
xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "remove Control = Control_L"
xmodmap -e "keysym Control_L = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Control_L"
xmodmap -e "add Lock = Caps_Lock"
xmodmap -e "add Control = Control_L"

term &

#exec enlightenment
xset b off
[ -x $(which unclutter 2>/dev/null) ] && unclutter -idle 5 -jitter 20&
exec ion3
#exec matchbox-session
#exec wmii
#exec twm
#exec startkde
