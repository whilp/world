xsetroot -solid black

# switch caps and control
xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "remove Control = Control_L"
xmodmap -e "keysym Control_L = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Control_L"
xmodmap -e "add Lock = Caps_Lock"
xmodmap -e "add Control = Control_L"

# Terminals
term -T MAIN -e 'screen -c /etc/screenrc -RD main' &
#[ -x "$(which pysh 2>/dev/null)" ] && term -T PY -e 'pysh' &

#exec enlightenment
xset b off
#[ -x $(which unclutter 2>/dev/null) ] && unclutter -idle 5 -jitter 20&

# Uggh -- opera yaks if ~/.fonts-cache* is there after a reboot.
[ -e $HOME/.fonts.cache-1 ] && rm -rf $HOME/.fonts.cache*

# Start kde crap; do this at the end to see if it increases
# performance?
#[ -x $(which konqueror) -a -x $(which kdeinit) ] && kdeinit&
exec ion3
#exec matchbox-session
#exec wmii
#exec twm
#exec startkde
