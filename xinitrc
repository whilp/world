xsetroot -solid black

# if [ -x $(which kdeinit) ]; then
#     kdeinit&
# fi

# switch caps and control
xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "remove Control = Control_L"
xmodmap -e "keysym Control_L = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Control_L"
xmodmap -e "add Lock = Caps_Lock"
xmodmap -e "add Control = Control_L"

xterm&

#exec enlightenment
xset b off
#unclutter -idle 5 -jitter 20&
exec ion3
#exec matchbox-session
#exec wmii
#xterm&
#exec twm
#exec startkde
